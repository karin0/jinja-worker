//! Rule lines in mihomo's syntax, and the expansion of rule sets into plain rules.

use std::sync::LazyLock;

use regex::Regex;
use serde::{Deserialize, Serialize};

/// Rule types whose payload may contain commas, so mihomo reads the target from the last field
/// and accepts no parameters after it.
const TARGET_LAST: [&str; 7] = [
    "AND",
    "OR",
    "NOT",
    "SUB-RULE",
    "DOMAIN-REGEX",
    "PROCESS-NAME-REGEX",
    "PROCESS-PATH-REGEX",
];

fn kind(line: &str) -> &str {
    line.split(',').next().unwrap_or_default().trim()
}

fn takes_params(kind: &str) -> bool {
    !TARGET_LAST.iter().any(|t| t.eq_ignore_ascii_case(kind))
}

/// Splits a matcher that may carry parameters, as a `classical` rule set entry does, at the
/// comma that ends its payload.
fn split_params(matcher: &str) -> (&str, &str) {
    if takes_params(kind(matcher))
        && let Some((at, _)) = matcher.match_indices(',').nth(1)
    {
        matcher.split_at(at)
    } else {
        (matcher, "")
    }
}

/// A rule line split the way mihomo parses it: `TYPE,PAYLOAD,TARGET[,PARAMS]`, `MATCH,TARGET`,
/// or a [`TARGET_LAST`] type ending with its target.
#[derive(Debug, PartialEq, Eq)]
pub struct Rule<'a> {
    /// `TYPE,PAYLOAD`, or `MATCH` alone.
    pub matcher: &'a str,
    pub target: &'a str,
    /// The parameters with their leading comma, or empty.
    pub params: &'a str,
}

impl<'a> Rule<'a> {
    /// Returns `None` for a line without a target field.
    pub fn parse(line: &'a str) -> Option<Self> {
        let kind = kind(line);
        if !takes_params(kind) {
            let (matcher, target) = line.rsplit_once(',')?;
            return Some(Self {
                matcher,
                target: target.trim(),
                params: "",
            });
        }
        let payload_fields = usize::from(!kind.eq_ignore_ascii_case("MATCH"));
        let (at, _) = line.match_indices(',').nth(payload_fields)?;
        let (matcher, rest) = (&line[..at], &line[at + 1..]);
        let (target, params) = rest.split_at(rest.find(',').unwrap_or(rest.len()));
        Some(Self {
            matcher,
            target: target.trim(),
            params,
        })
    }

    pub fn kind(&self) -> &'a str {
        kind(self.matcher)
    }

    /// Whether the target names an outbound. A `SUB-RULE` targets a sub-rule instead.
    pub fn targets_outbound(&self) -> bool {
        !self.kind().eq_ignore_ascii_case("SUB-RULE")
    }

    /// This rule with its matcher replaced by `matcher`, whose own parameters follow the rule's.
    fn with_matcher(&self, matcher: &str) -> String {
        let (matcher, matcher_params) = split_params(matcher);
        if takes_params(kind(matcher)) {
            format!("{matcher},{}{}{matcher_params}", self.target, self.params)
        } else {
            format!("{matcher},{}", self.target)
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Behavior {
    Domain,
    Ipcidr,
    Classical,
}

impl Behavior {
    /// The matcher for one payload entry of a rule set with this behavior.
    fn matcher(self, entry: &str) -> String {
        match self {
            Self::Domain => domain_matcher(entry),
            Self::Ipcidr => format!("IP-CIDR,{entry}"),
            Self::Classical => entry.to_owned(),
        }
    }
}

/// Translates mihomo's domain set syntax: `+.` matches the domain and all its subdomains, a
/// leading `.` only the subdomains, and a `*` label any single label.
fn domain_matcher(entry: &str) -> String {
    let has_wildcard = |domain: &str| domain.split('.').any(|label| label == "*");
    if let Some(domain) = entry.strip_prefix("+.")
        && !has_wildcard(domain)
    {
        return format!("DOMAIN-SUFFIX,{domain}");
    }
    if !entry.starts_with('.') && !has_wildcard(entry) {
        return format!("DOMAIN,{entry}");
    }
    let (prefix, domain) = entry
        .strip_prefix("+.")
        .map(|domain| (r"(?:.+\.)?", domain))
        .or_else(|| entry.strip_prefix('.').map(|domain| (r".+\.", domain)))
        .unwrap_or(("", entry));
    let labels: Vec<String> = domain
        .split('.')
        .map(|label| {
            if label == "*" {
                "[^.]+".to_owned()
            } else {
                regex::escape(label)
            }
        })
        .collect();
    format!("DOMAIN-REGEX,^{prefix}{}$", labels.join(r"\."))
}

static RULE_SET: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"RULE-SET,([^(),]+)").expect("constant regex"));

/// The first `RULE-SET` reference in a rule line, either the rule itself or a sub-rule of a
/// logic rule.
pub struct RuleSetRef<'a> {
    line: &'a str,
    start: usize,
    end: usize,
    pub id: &'a str,
}

impl<'a> RuleSetRef<'a> {
    pub fn find(line: &'a str) -> Option<Self> {
        let cap = RULE_SET.captures(line)?;
        let all = cap.get(0)?;
        Some(Self {
            line,
            start: all.start(),
            end: all.end(),
            id: cap.get(1)?.as_str(),
        })
    }

    /// One rule per entry of the referenced set. A top-level reference takes the rule's target
    /// and parameters, and a sub-rule reference is replaced in place.
    pub fn expand(&self, behavior: Behavior, entries: &[String]) -> Vec<String> {
        let rule = (self.start == 0).then(|| Rule::parse(self.line)).flatten();
        entries
            .iter()
            .map(|entry| {
                let matcher = behavior.matcher(entry);
                match &rule {
                    Some(rule) => rule.with_matcher(&matcher),
                    None => format!(
                        "{}{matcher}{}",
                        &self.line[..self.start],
                        &self.line[self.end..]
                    ),
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(line: &str) -> (&str, &str, &str) {
        let r = Rule::parse(line).unwrap();
        (r.matcher, r.target, r.params)
    }

    #[test]
    fn target_follows_payload() {
        assert_eq!(
            rule("IP-CIDR,1.2.3.0/24,PROXY,no-resolve"),
            ("IP-CIDR,1.2.3.0/24", "PROXY", ",no-resolve")
        );
        assert_eq!(rule("DOMAIN,a.com,DIRECT"), ("DOMAIN,a.com", "DIRECT", ""));
    }

    #[test]
    fn match_has_no_payload() {
        assert_eq!(rule("MATCH,UNMATCHED"), ("MATCH", "UNMATCHED", ""));
    }

    #[test]
    fn logic_and_regex_rules_end_with_target() {
        assert_eq!(
            rule("AND,((DOMAIN,a.com),(NETWORK,UDP)),REJECT"),
            ("AND,((DOMAIN,a.com),(NETWORK,UDP))", "REJECT", "")
        );
        assert_eq!(
            rule("DOMAIN-REGEX,^a{1,2}\\.com$,PROXY"),
            ("DOMAIN-REGEX,^a{1,2}\\.com$", "PROXY", "")
        );
        assert!(
            !Rule::parse("SUB-RULE,(NETWORK,TCP),sub")
                .unwrap()
                .targets_outbound()
        );
    }

    #[test]
    fn missing_target() {
        assert_eq!(Rule::parse("DOMAIN,a.com"), None);
        assert_eq!(Rule::parse("MATCH"), None);
    }

    fn expand(line: &str, behavior: Behavior, entries: &[&str]) -> Vec<String> {
        let entries: Vec<String> = entries.iter().map(|&e| e.to_owned()).collect();
        RuleSetRef::find(line).unwrap().expand(behavior, &entries)
    }

    #[test]
    fn domain_set_wildcards() {
        assert_eq!(
            expand(
                "RULE-SET,ads,REJECT",
                Behavior::Domain,
                &["a.com", "+.b.com", ".c.com", "*.d.com", "+.*.e.com"]
            ),
            [
                "DOMAIN,a.com,REJECT",
                "DOMAIN-SUFFIX,b.com,REJECT",
                r"DOMAIN-REGEX,^.+\.c\.com$,REJECT",
                r"DOMAIN-REGEX,^[^.]+\.d\.com$,REJECT",
                r"DOMAIN-REGEX,^(?:.+\.)?[^.]+\.e\.com$,REJECT",
            ]
        );
    }

    #[test]
    fn rule_params_follow_target() {
        assert_eq!(
            expand(
                "RULE-SET,cn,DIRECT,no-resolve",
                Behavior::Ipcidr,
                &["1.0.0.0/8"]
            ),
            ["IP-CIDR,1.0.0.0/8,DIRECT,no-resolve"]
        );
        // A regex rule takes no parameters.
        assert_eq!(
            expand(
                "RULE-SET,s,PROXY,no-resolve",
                Behavior::Domain,
                &["*.a.com"]
            ),
            [r"DOMAIN-REGEX,^[^.]+\.a\.com$,PROXY"]
        );
    }

    #[test]
    fn classical_entry_params_follow_target() {
        assert_eq!(
            expand(
                "RULE-SET,s,PROXY",
                Behavior::Classical,
                &[
                    "IP-CIDR,1.0.0.0/8,no-resolve",
                    "AND,((NETWORK,UDP),(DST-PORT,443))"
                ]
            ),
            [
                "IP-CIDR,1.0.0.0/8,PROXY,no-resolve",
                "AND,((NETWORK,UDP),(DST-PORT,443)),PROXY",
            ]
        );
    }

    #[test]
    fn sub_rule_reference_is_replaced_in_place() {
        assert_eq!(
            expand(
                "AND,((RULE-SET,s),(NETWORK,UDP)),REJECT",
                Behavior::Classical,
                &["IP-CIDR,1.0.0.0/8,no-resolve"]
            ),
            ["AND,((IP-CIDR,1.0.0.0/8,no-resolve),(NETWORK,UDP)),REJECT"]
        );
    }
}
