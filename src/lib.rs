mod profile;
mod rules;

use std::collections::BTreeMap;
use std::pin::pin;
use std::time::Duration;

use futures::future::{Either, join_all, select};
use indexmap::IndexMap;
use profile::{Content, Profile, Proxy, Remote};
use rules::Behavior;
use worker::{
    Context, Delay, Env, Fetch, Headers, Request, RequestInit, Response, ResponseBuilder, Url,
    console_error, console_log, console_warn, event,
};

const USER_AGENT: &str = "ClashMetaForGenshinImpact/5.0";
const TIMEOUT: Duration = Duration::from_secs(10);

/// Fetches `url` and returns its body when the status is 2xx, along with the response headers.
async fn get(url: &str, headers: &[(&str, &str)]) -> Result<(String, Headers), profile::Error> {
    let fetch = async {
        let h = Headers::new();
        h.set("User-Agent", USER_AGENT)?;
        for (k, v) in headers {
            h.set(k, v)?;
        }
        let req = Request::new_with_init(url, RequestInit::new().with_headers(h))?;
        let mut resp = Fetch::Request(req).send().await?;
        let status = resp.status_code();
        if !(200..300).contains(&status) {
            return Err(format!("HTTP {status}").into());
        }
        Ok((resp.text().await?, resp.headers().clone()))
    };
    match select(pin!(fetch), Delay::from(TIMEOUT)).await {
        Either::Left((result, _)) => result,
        Either::Right(_) => Err(format!("timed out after {TIMEOUT:?}").into()),
    }
}

async fn get_template(env: &Env) -> Result<String, profile::Error> {
    let url = format!(
        "https://api.github.com/repos/{}/contents/{}",
        env.secret("GH_REPO")?,
        env.secret("GH_PATH")?
    );
    let auth = format!("Bearer {}", env.secret("GH_TOKEN")?);
    let headers = [
        ("Authorization", auth.as_str()),
        ("Accept", "application/vnd.github.raw+json"),
        ("X-GitHub-Api-Version", "2022-11-28"),
    ];
    Ok(get(&url, &headers).await?.0)
}

enum Provided {
    Proxies(Vec<Proxy>, Option<String>),
    Rules(Behavior, Vec<String>),
}

async fn get_provider(remote: &Remote) -> Result<Provided, profile::Error> {
    let (body, headers) = get(&remote.url, &[]).await?;
    Ok(match remote.content {
        Content::Proxies => Provided::Proxies(
            profile::parse_proxies(&body)?,
            headers
                .get("subscription-userinfo")?
                .filter(|s| !s.is_empty()),
        ),
        Content::Rules(behavior) => Provided::Rules(behavior, profile::parse_rule_payload(&body)?),
    })
}

async fn subscription(template: &str, cmd: &str, ua: &str) -> Result<Response, profile::Error> {
    let mut profile = Profile::parse(&profile::render(template, cmd, ua)?)?;
    let remotes = profile.take_remotes();
    let results = join_all(remotes.iter().map(get_provider)).await;

    let mut proxies = IndexMap::new();
    let mut rule_sets = BTreeMap::new();
    let mut sub_info = None;
    for (remote, result) in remotes.into_iter().zip(results) {
        match result {
            Ok(Provided::Proxies(list, info)) => {
                if sub_info.is_none() {
                    sub_info = info;
                }
                proxies.insert(remote.id, list);
            }
            Ok(Provided::Rules(behavior, payload)) => {
                rule_sets.insert(remote.id, (behavior, payload));
            }
            Err(e) => console_warn!("provider {} skipped: {}", remote.id, e),
        }
    }

    let mut resp = ResponseBuilder::new();
    if let Some(info) = sub_info {
        resp = resp.with_header("subscription-userinfo", &info)?;
    }
    Ok(resp.ok(profile.resolve(proxies, &rule_sets)?)?)
}

/// The `cmd` segment of a `/<access path>/<cmd>` request path.
fn command<'a>(url: &'a Url, access_path: &str) -> Option<&'a str> {
    let mut parts = url.path_segments()?;
    match (parts.next(), parts.next(), parts.next()) {
        (Some(access), Some(cmd), None) if access == access_path => Some(cmd),
        _ => None,
    }
}

#[event(fetch)]
async fn fetch(req: Request, env: Env, _ctx: Context) -> worker::Result<Response> {
    console_error_panic_hook::set_once();

    let url = req.url()?;
    let Some(cmd) = command(&url, &env.secret("ACCESS_PATH")?.to_string()) else {
        console_warn!("bad request: {}", url.path());
        return Response::error("Not found", 404);
    };
    let ua = req.headers().get("User-Agent")?.unwrap_or_default();
    console_log!("cmd: {} ua: {}", cmd, ua);

    let template = match get_template(&env).await {
        Ok(t) => t,
        Err(e) => {
            console_error!("template: {}", e);
            return Response::error(format!("template unavailable: {e}"), 502);
        }
    };
    match subscription(&template, cmd, &ua).await {
        Ok(resp) => Ok(resp),
        Err(e) => {
            console_error!("{}", e);
            Response::error(e.to_string(), 500)
        }
    }
}
