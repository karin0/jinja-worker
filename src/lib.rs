mod rules;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::mem::take;

use futures::future::join_all;
use indexmap::IndexMap;
use minijinja::Environment;
use regex::Regex;
use rules::{Behavior, Rule, RuleSetRef};
use serde::{Deserialize, Serialize};
use worker::{
    Context, Env, Fetch, Method, Request, Response, ResponseBuilder, Result, console_log,
    console_warn, event,
};

const USER_AGENT: &str = "ClashMetaForGenshinImpact/5.0";

async fn get_tmpl(env: &Env) -> String {
    let token = env.secret("GH_TOKEN").unwrap();
    let url = format!(
        "https://api.github.com/repos/{}/contents/{}",
        env.secret("GH_REPO").unwrap(),
        env.secret("GH_PATH").unwrap()
    );
    let mut req = Request::new(&url, Method::Get).unwrap();
    let headers = req.headers_mut().unwrap();
    headers
        .append("Authorization", format!("Bearer {token}").as_str())
        .unwrap();
    headers
        .append("Accept", "application/vnd.github.raw+json")
        .unwrap();
    headers
        .append("X-GitHub-Api-Version", "2022-11-28")
        .unwrap();
    headers.append("User-Agent", USER_AGENT).unwrap();

    Fetch::Request(req)
        .send()
        .await
        .unwrap()
        .text()
        .await
        .unwrap()
}

async fn fetch_provider(
    url: String,
    id: String,
    rule_behavior: Option<Behavior>,
) -> (String, String, Option<Behavior>, Option<String>) {
    let mut req = Request::new(&url, Method::Get).unwrap();
    let headers = req.headers_mut().unwrap();
    headers.append("User-Agent", USER_AGENT).unwrap();
    let mut r = Fetch::Request(req).send().await.unwrap();
    let mut info = None;
    if rule_behavior.is_none()
        && let Ok(Some(s)) = r.headers().get("subscription-userinfo")
    {
        console_log!("{}: {}", id, s);
        if !s.is_empty() {
            info = Some(s);
        }
    }
    (r.text().await.unwrap(), id, rule_behavior, info)
}

#[derive(Deserialize)]
struct MetaInfo {
    #[serde(rename = "x-deps")]
    deps: BTreeMap<String, String>,
}

#[derive(Deserialize, Debug)]
struct RenderedInfo {
    #[serde(rename = "x-repl")]
    #[serde(default)]
    repl: BTreeMap<String, String>,
}

#[derive(Serialize, Deserialize, Debug)]
struct ProxyProvider {
    #[serde(rename = "type")]
    typ: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(flatten)]
    extra: IndexMap<String, yaml_serde::Value>,
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
    extra: IndexMap<String, yaml_serde::Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Proxy {
    name: String,
    #[serde(flatten)]
    extra: IndexMap<String, yaml_serde::Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct ProxyGroup<'a> {
    name: String,
    #[serde(rename = "use")]
    #[serde(skip_serializing_if = "Option::is_none")]
    uses: Option<Vec<String>>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    proxies: Vec<Cow<'a, str>>,
    #[serde(flatten)]
    extra: IndexMap<String, yaml_serde::Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Profile<'a> {
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    proxies: Vec<Cow<'a, Proxy>>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    rules: Vec<String>,
    #[serde(rename = "proxy-groups")]
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    proxy_groups: Vec<ProxyGroup<'a>>,
    // Preserve the order via IndexMap to get expected 'subscription-userinfo'.
    #[serde(rename = "proxy-providers")]
    #[serde(default)]
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    proxy_providers: IndexMap<String, ProxyProvider>,
    #[serde(rename = "rule-providers")]
    #[serde(default)]
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    rule_providers: IndexMap<String, RuleProvider>,
    #[serde(skip_serializing)]
    #[serde(flatten)]
    _rendered_info: RenderedInfo,
    #[serde(flatten)]
    extra: IndexMap<String, yaml_serde::Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct ProxyContent {
    proxies: Vec<Proxy>,
}

#[derive(Serialize, Deserialize, Debug)]
struct RuleContent {
    payload: Vec<String>,
}

#[event(fetch)]
async fn fetch(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let url = req.url().unwrap();
    let mut parts = url.path_segments().unwrap();
    if parts.next().unwrap() != env.secret("ACCESS_PATH").unwrap().to_string() {
        console_warn!("Bad request: {}", url);
        return Response::error("Not found", 404);
    }
    let cmd = parts.next().unwrap();
    if parts.next().is_some() {
        console_warn!("Bad request: {}", url);
        return Response::error("Not found", 404);
    }

    let mut ua = req.headers().get("User-Agent").unwrap().unwrap();
    console_log!("cmd: {} ua: {}", cmd, ua);

    ua = ua.to_ascii_lowercase();
    let is_pc = ua.contains("clash-verge") || ua.contains("clashforwindows");

    let mut ctx = BTreeMap::new();
    let yes = 1;
    if is_pc {
        ctx.insert("pc", yes);
    }
    for c in cmd.split('-') {
        ctx.insert(c, yes);
    }

    let tmpl_full = get_tmpl(&env).await;
    let mut tmpl = tmpl_full.as_str();
    let conf = tmpl.find("\n\n").and_then(|p| {
        if let Ok(y) = yaml_serde::from_str::<MetaInfo>(&tmpl[..p]) {
            tmpl = &tmpl[p + 2..];
            Some(y)
        } else {
            None
        }
    });

    let mut deps: Vec<(Vec<&str>, Vec<&str>)> = Vec::new();
    if let Some(y) = &conf {
        for (k, v) in &y.deps {
            deps.push((k.split('-').collect(), v.split('-').collect()));
        }
    }

    console_log!("deps: {:?}", deps);
    loop {
        let mut changed = false;
        for (k, v) in &deps {
            if k.iter().all(|c| ctx.contains_key(c)) {
                for c in v {
                    if ctx.insert(c, yes).is_none() {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    console_log!("ctx: {:?}", ctx);
    let mut env = Environment::new();
    env.set_trim_blocks(true);

    let mut r = env.render_str(tmpl, ctx).unwrap();

    if let Ok(rendered_info) = yaml_serde::from_str::<RenderedInfo>(&r) {
        for (k, v) in &rendered_info.repl {
            if let Ok(reg) = Regex::new(k) {
                console_log!("replace: {:?} -> {:?}", k, v);
                r = reg.replace_all(&r, v).to_string();
            } else {
                console_warn!("bad regex: {}", k);
            }
        }
    }

    let mut profile = yaml_serde::from_str::<Profile>(&r).unwrap();

    let mut proxy_names = BTreeSet::new();
    for p in &profile.proxies {
        proxy_names.insert(p.name.as_str());
    }

    let mut futs = vec![];
    profile.proxy_providers.retain(|id, p| {
        if p.typ == "http" {
            futs.push(fetch_provider(take(&mut p.url).unwrap(), id.clone(), None));
            false
        } else {
            true
        }
    });
    profile.rule_providers.retain(|id, p| {
        if p.typ == "http" {
            if let Some(format) = &p.format
                && format != "yaml"
            {
                return true;
            }
            futs.push(fetch_provider(
                take(&mut p.url).unwrap(),
                id.clone(),
                Some(p.behavior),
            ));
            false
        } else {
            true
        }
    });

    console_log!("fetching {} providers", futs.len());

    let mut proxy_contents = BTreeMap::new();
    let mut rule_contents = BTreeMap::new();
    let mut sub_info = None;
    for (content, name, behavior, info) in join_all(futs).await {
        if let Some(behavior) = behavior {
            rule_contents.insert(
                name,
                (
                    behavior,
                    yaml_serde::from_str::<RuleContent>(&content)
                        .unwrap()
                        .payload,
                ),
            );
        } else {
            if let (None, Some(info)) = (&sub_info, info) {
                console_log!("sub_info: {} from {}", info, name);
                sub_info = Some(info);
            }
            proxy_contents.insert(
                name,
                yaml_serde::from_str::<ProxyContent>(&content)
                    .unwrap()
                    .proxies,
            );
        }
    }

    for c in proxy_contents.values_mut() {
        for p in c.iter_mut() {
            while proxy_names.contains(p.name.as_str()) {
                p.name.push('_');
            }
            proxy_names.insert(p.name.as_str());
        }
    }

    // Gather all outbound names.
    let mut outbounds = proxy_names;
    for name in ["DIRECT", "REJECT", "REJECT-DROP", "PASS", "COMPATIBLE"] {
        outbounds.insert(name);
    }
    for g in &mut profile.proxy_groups {
        outbounds.insert(&g.name);
    }

    // Drop rules with unknown outbounds.
    profile.rules.retain(|rule| {
        Rule::parse(rule).is_none_or(|r| !r.targets_outbound() || outbounds.contains(r.target))
    });
    drop(outbounds);

    if !proxy_contents.is_empty() {
        for g in &mut profile.proxy_groups {
            if let Some(uses) = &mut g.uses {
                uses.retain(|u| {
                    if let Some(c) = proxy_contents.get(u.as_str()) {
                        for p in c {
                            g.proxies.push(Cow::Borrowed(p.name.as_str()));
                        }
                        false
                    } else {
                        true
                    }
                });
            }
        }

        for c in proxy_contents.values() {
            for p in c {
                profile.proxies.push(Cow::Borrowed(p));
            }
        }
    }

    for rule in take(&mut profile.rules) {
        if let Some(set) = RuleSetRef::find(&rule)
            && let Some((behavior, entries)) = rule_contents.get(set.id)
        {
            profile.rules.extend(set.expand(*behavior, entries));
        } else {
            profile.rules.push(rule);
        }
    }

    let out = yaml_serde::to_string(&profile).unwrap();
    let mut r = ResponseBuilder::new();
    if let Some(info) = sub_info {
        r = r.with_header("subscription-userinfo", &info).unwrap();
    }
    r.ok(out)
}
