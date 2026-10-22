//! Rendering a template into a mihomo profile and inlining the providers it names.

use std::collections::{BTreeMap, BTreeSet};
use std::mem::take;

use indexmap::IndexMap;
use minijinja::Environment;
use regex::Regex;
use serde::de::IgnoredAny;
use serde::{Deserialize, Serialize};
use yaml_serde::Value;

use crate::rules::{Behavior, Rule, RuleSetRef};

pub type Error = Box<dyn std::error::Error>;

const BUILTIN_OUTBOUNDS: [&str; 5] = ["DIRECT", "REJECT", "REJECT-DROP", "PASS", "COMPATIBLE"];

/// The template's header paragraph, which maps a `-`-joined flag set to the flags it implies.
#[derive(Deserialize)]
struct Header {
    #[serde(rename = "x-deps")]
    deps: BTreeMap<String, String>,
}

/// Regex replacements over the rendered text, keyed by pattern.
#[derive(Deserialize)]
struct Replacements {
    #[serde(rename = "x-repl", default)]
    repl: BTreeMap<String, String>,
}

/// Renders `template` for the `-`-joined flags in `cmd` plus those the User-Agent implies.
pub fn render(template: &str, cmd: &str, user_agent: &str) -> Result<String, Error> {
    let (deps, body) = template
        .split_once("\n\n")
        .and_then(|(head, body)| {
            yaml_serde::from_str::<Header>(head)
                .ok()
                .map(|h| (h.deps, body))
        })
        .unwrap_or_else(|| (BTreeMap::new(), template));
    let deps: Vec<(Vec<&str>, Vec<&str>)> = deps
        .iter()
        .map(|(k, v)| (k.split('-').collect(), v.split('-').collect()))
        .collect();

    let mut flags: BTreeSet<&str> = cmd.split('-').collect();
    let ua = user_agent.to_ascii_lowercase();
    if ua.contains("clash-verge") || ua.contains("clashforwindows") {
        flags.insert("pc");
    }
    loop {
        let before = flags.len();
        for (cond, implied) in &deps {
            if cond.iter().all(|c| flags.contains(c)) {
                flags.extend(implied);
            }
        }
        if flags.len() == before {
            break;
        }
    }

    let mut env = Environment::new();
    env.set_trim_blocks(true);
    let ctx: BTreeMap<&str, u8> = flags.into_iter().map(|f| (f, 1)).collect();
    let mut rendered = env.render_str(body, ctx)?;

    for (pattern, replacement) in yaml_serde::from_str::<Replacements>(&rendered)?.repl {
        rendered = Regex::new(&pattern)?
            .replace_all(&rendered, replacement.as_str())
            .into_owned();
    }
    Ok(rendered)
}

#[derive(Serialize, Deserialize, Debug)]
struct ProxyProvider {
    #[serde(rename = "type")]
    typ: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(flatten)]
    extra: IndexMap<String, Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct RuleProvider {
    #[serde(rename = "type")]
    typ: String,
    behavior: Behavior,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    format: Option<String>,
    #[serde(flatten)]
    extra: IndexMap<String, Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Proxy {
    name: String,
    #[serde(flatten)]
    extra: IndexMap<String, Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct ProxyGroup {
    name: String,
    #[serde(rename = "use", default, skip_serializing_if = "Vec::is_empty")]
    uses: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    proxies: Vec<String>,
    #[serde(flatten)]
    extra: IndexMap<String, Value>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Profile {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    proxies: Vec<Proxy>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    rules: Vec<String>,
    #[serde(
        rename = "proxy-groups",
        default,
        skip_serializing_if = "Vec::is_empty"
    )]
    proxy_groups: Vec<ProxyGroup>,
    // Template order decides which provider's subscription-userinfo the response carries.
    #[serde(
        rename = "proxy-providers",
        default,
        skip_serializing_if = "IndexMap::is_empty"
    )]
    proxy_providers: IndexMap<String, ProxyProvider>,
    #[serde(
        rename = "rule-providers",
        default,
        skip_serializing_if = "IndexMap::is_empty"
    )]
    rule_providers: IndexMap<String, RuleProvider>,
    #[serde(rename = "x-repl", default, skip_serializing)]
    _repl: Option<IgnoredAny>,
    #[serde(flatten)]
    extra: IndexMap<String, Value>,
}

/// What a remote provider serves.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Content {
    Proxies,
    Rules(Behavior),
}

/// A provider for the worker to fetch and inline, in template order.
#[derive(Debug, PartialEq, Eq)]
pub struct Remote {
    pub id: String,
    pub url: String,
    pub content: Content,
}

#[derive(Deserialize)]
struct ProxyList {
    proxies: Vec<Proxy>,
}

#[derive(Deserialize)]
struct RulePayload {
    payload: Vec<String>,
}

pub fn parse_proxies(body: &str) -> Result<Vec<Proxy>, Error> {
    Ok(yaml_serde::from_str::<ProxyList>(body)?.proxies)
}

pub fn parse_rule_payload(body: &str) -> Result<Vec<String>, Error> {
    Ok(yaml_serde::from_str::<RulePayload>(body)?.payload)
}

impl Profile {
    pub fn parse(rendered: &str) -> Result<Self, Error> {
        Ok(yaml_serde::from_str(rendered)?)
    }

    /// Removes the HTTP providers the worker inlines: every proxy provider and the YAML rule
    /// providers.
    pub fn take_remotes(&mut self) -> Vec<Remote> {
        let mut remotes = Vec::new();
        self.proxy_providers.retain(|id, p| match &p.url {
            Some(url) if p.typ == "http" => {
                remotes.push(Remote {
                    id: id.clone(),
                    url: url.clone(),
                    content: Content::Proxies,
                });
                false
            }
            _ => true,
        });
        self.rule_providers.retain(|id, p| match &p.url {
            Some(url) if p.typ == "http" && p.format.as_deref().is_none_or(|f| f == "yaml") => {
                remotes.push(Remote {
                    id: id.clone(),
                    url: url.clone(),
                    content: Content::Rules(p.behavior),
                });
                false
            }
            _ => true,
        });
        remotes
    }

    /// Inlines the fetched providers and serializes the profile. A taken provider missing from
    /// `proxies` or `rule_sets` failed, so the groups and rules that name it lose that reference,
    /// and a group left without members falls back to `COMPATIBLE` as mihomo's own empty groups
    /// do.
    pub fn resolve(
        mut self,
        mut proxies: IndexMap<String, Vec<Proxy>>,
        rule_sets: &BTreeMap<String, (Behavior, Vec<String>)>,
    ) -> Result<String, Error> {
        let mut names: BTreeSet<String> = self.proxies.iter().map(|p| p.name.clone()).collect();
        for p in proxies.values_mut().flatten() {
            while names.contains(&p.name) {
                p.name.push('_');
            }
            names.insert(p.name.clone());
        }

        for g in &mut self.proxy_groups {
            for u in take(&mut g.uses) {
                if let Some(list) = proxies.get(&u) {
                    g.proxies.extend(list.iter().map(|p| p.name.clone()));
                } else if self.proxy_providers.contains_key(&u) {
                    g.uses.push(u);
                }
            }
            if g.uses.is_empty() && g.proxies.is_empty() {
                g.proxies.push("COMPATIBLE".to_owned());
            }
        }

        let mut outbounds: BTreeSet<&str> = BUILTIN_OUTBOUNDS.into();
        outbounds.extend(names.iter().map(String::as_str));
        outbounds.extend(self.proxy_groups.iter().map(|g| g.name.as_str()));
        self.rules.retain(|rule| {
            Rule::parse(rule).is_none_or(|r| !r.targets_outbound() || outbounds.contains(r.target))
        });

        for rule in take(&mut self.rules) {
            match RuleSetRef::find(&rule) {
                Some(set) => {
                    if let Some((behavior, entries)) = rule_sets.get(set.id) {
                        self.rules.extend(set.expand(*behavior, entries));
                    } else if self.rule_providers.contains_key(set.id) {
                        self.rules.push(rule);
                    }
                }
                None => self.rules.push(rule),
            }
        }

        self.proxies.extend(proxies.into_values().flatten());
        Ok(yaml_serde::to_string(&self)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEMPLATE: &str = "x-deps:
  win: pc

{% if pc %}
pc: true
{% endif %}
proxies:
  - name: a
    type: direct
proxy-groups:
  - name: PROXY
    type: select
    use: [p1, p2]
  - name: ALT
    type: select
    use: [p2]
proxy-providers:
  p1:
    type: http
    url: https://p1.example
  p2:
    type: http
    url: https://p2.example
  local:
    type: file
    path: ./local.yml
rule-providers:
  r1:
    type: http
    behavior: domain
    url: https://r1.example
  r2:
    type: http
    behavior: ipcidr
    url: https://r2.example
rules:
  - RULE-SET,r1,PROXY
  - RULE-SET,r2,PROXY,no-resolve
  - IP-CIDR,10.0.0.0/8,PROXY,no-resolve
  - DOMAIN,a.com,MISSING
  - MATCH,PROXY
";

    fn profile(cmd: &str, ua: &str) -> Profile {
        Profile::parse(&render(TEMPLATE, cmd, ua).unwrap()).unwrap()
    }

    #[test]
    fn flags_close_over_header_and_user_agent() {
        assert!(profile("win", "").extra.contains_key("pc"));
        assert!(profile("x", "Clash-Verge/2.0").extra.contains_key("pc"));
        assert!(!profile("x", "").extra.contains_key("pc"));
    }

    #[test]
    fn remotes_follow_template_order() {
        let ids: Vec<String> = profile("x", "")
            .take_remotes()
            .into_iter()
            .map(|r| r.id)
            .collect();
        assert_eq!(ids, ["p1", "p2", "r1", "r2"]);
    }

    fn resolve(fetched: &[&str]) -> Profile {
        let mut p = profile("x", "");
        p.take_remotes();
        let proxy = |name: &str| Proxy {
            name: name.to_owned(),
            extra: IndexMap::new(),
        };
        let proxies = [
            ("p1", vec![proxy("a"), proxy("b")]),
            ("p2", vec![proxy("c")]),
        ]
        .into_iter()
        .filter(|(id, _)| fetched.contains(id))
        .map(|(id, list)| (id.to_owned(), list))
        .collect();
        let rule_sets = [
            ("r1", (Behavior::Domain, vec!["+.b.com".to_owned()])),
            ("r2", (Behavior::Ipcidr, vec!["1.0.0.0/8".to_owned()])),
        ]
        .into_iter()
        .filter(|(id, _)| fetched.contains(id))
        .map(|(id, set)| (id.to_owned(), set))
        .collect();
        Profile::parse(&p.resolve(proxies, &rule_sets).unwrap()).unwrap()
    }

    fn group(p: &Profile, name: &str) -> (Vec<String>, Vec<String>) {
        let g = p.proxy_groups.iter().find(|g| g.name == name).unwrap();
        (g.uses.clone(), g.proxies.clone())
    }

    #[test]
    fn inlines_fetched_providers() {
        let p = resolve(&["p1", "p2", "r1", "r2"]);
        let names: Vec<&str> = p.proxies.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, ["a", "a_", "b", "c"]);
        assert_eq!(
            group(&p, "PROXY"),
            (vec![], vec!["a_".into(), "b".into(), "c".into()])
        );
        assert_eq!(
            p.rules,
            [
                "DOMAIN-SUFFIX,b.com,PROXY",
                "IP-CIDR,1.0.0.0/8,PROXY,no-resolve",
                "IP-CIDR,10.0.0.0/8,PROXY,no-resolve",
                "MATCH,PROXY",
            ]
        );
        assert_eq!(p.proxy_providers.keys().collect::<Vec<_>>(), ["local"]);
        assert!(p.rule_providers.is_empty());
    }

    #[test]
    fn failed_providers_lose_their_references() {
        let p = resolve(&["p1", "r2"]);
        assert_eq!(group(&p, "PROXY"), (vec![], vec!["a_".into(), "b".into()]));
        assert_eq!(group(&p, "ALT"), (vec![], vec!["COMPATIBLE".into()]));
        assert_eq!(
            p.rules,
            [
                "IP-CIDR,1.0.0.0/8,PROXY,no-resolve",
                "IP-CIDR,10.0.0.0/8,PROXY,no-resolve",
                "MATCH,PROXY",
            ]
        );
    }
}
