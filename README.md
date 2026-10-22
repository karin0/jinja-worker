# jinja-worker

A Cloudflare Worker that renders a [minijinja](https://github.com/mitsuhiko/minijinja) template from a GitHub repository into a [mihomo](https://github.com/MetaCubeX/mihomo) profile and serves it as a proxy subscription.

## Requests

A request path is `/<ACCESS_PATH>/<cmd>`, where `cmd` is a list of flags joined by `-`. Any other path gets a 404. A User-Agent containing `clash-verge` or `clashforwindows` adds the `pc` flag. The template renders with every flag set to `1`.

The template may open with a YAML paragraph, ended by a blank line, whose `x-deps` map closes the flag set over implications: a key of `-`-joined flags, once all present, adds the flags of its value. A rendered `x-repl` map applies regex replacements to the rendered text before the worker parses it.

## Providers

The worker fetches every HTTP proxy provider and every HTTP rule provider in YAML format, and inlines them into the profile:

- the proxies join the profile, renamed with a `_` suffix on a name clash, and replace the provider in each group's `use`
- a `RULE-SET` rule becomes one rule per entry of the set, with mihomo's domain wildcards (`+.`, a leading `.`, `*`) translated into `DOMAIN-SUFFIX` or `DOMAIN-REGEX` rules
- the response carries the `subscription-userinfo` header of the first proxy provider, in template order, that sends one

A provider that fails to fetch or parse is skipped with a warning. The groups and rules that name it lose the reference, and a group left without members falls back to `COMPATIBLE`.

After inlining, the worker drops every rule whose target is neither a proxy, a group, nor a built-in outbound, so a template can route to a group that only some flag sets define.

## Deployment

The worker reads these secrets:

| secret | value |
| --- | --- |
| `ACCESS_PATH` | the first path segment a request must carry |
| `GH_TOKEN` | a GitHub token that can read the template |
| `GH_REPO` | the template repository, as `owner/name` |
| `GH_PATH` | the template path inside that repository |

```sh
pnpm install
pnpm wrangler secret put GH_TOKEN   # and the other secrets
pnpm wrangler deploy                # builds with worker-build, which needs the wasm32-unknown-unknown target
```

## Development

```sh
cargo fmt --check
cargo clippy --all-targets
cargo clippy --target wasm32-unknown-unknown
cargo test
```

GitHub Actions runs these on every push. `pnpm-workspace.yaml` makes pnpm refuse package versions published within the last 15 days.
