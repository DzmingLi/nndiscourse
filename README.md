# nndiscourse

Gnus backend for Discourse topics, category lists, and replies to you. This is a GPL-3.0-or-later fork of [dickmao/nndiscourse](https://github.com/dickmao/nndiscourse), retaining the upstream Git history. It uses native Emacs HTTP and Discourse User API Keys.

Each topic URL you subscribe to becomes one Gnus group (`topic.ID`); its posts are native articles with stable, site-scoped Message-IDs and References. A `category.ID.slug` group imports topic roots only; Gnus `A T` retrieves the selected topic's replies on demand. A `notifications` group combines replies to you in the same topic into one current article. Gnus owns subscriptions, read/unread/ticked marks and cache. Unsubscribing with Gnus `u` retains local read state; later scans skip that group.

## Requirements

Emacs 29.1+, [plz](https://github.com/alphapapa/plz.el) 0.9.1+, curl, and OpenSSL. `nndiscourse.el` and `discourse-auth.el` are installed together. The browser is needed to approve a User API Key once; API calls do not use browser cookies. Public topics can be read anonymously. Posting requires a User API Key with write permission.

Clone this repository and put its root on `load-path`:

```elisp
(add-to-list 'load-path "/path/to/nndiscourse")
(require 'nndiscourse)
(setq discourse-auth-openssl-program "/path/to/openssl") ; if not on PATH
```

`M-x nndiscourse-subscribe-topic` accepts a Discourse topic URL such as `https://discourse.nixos.org/t/52296`. It registers exactly that topic in Gnus, fetches the full topic, and opens its group. Subsequent Gnus scans query subscribed topics only. Topic refresh is also available through normal Gnus `g`/`M-g` commands. `M-x discourse-auth-login` authorizes the site for private reads and posting.

Category groups use the name `category.ID.slug` and a site-specific method, for example `(nndiscourse "emacs-china.org" (nndiscourse-address "https://emacs-china.org"))` with group `category.8.org-mode`. The category's own JSON endpoint supplies only topic roots. Use Gnus `A T` on a root to load its replies. `M-x nndiscourse-subscribe-topic-at-point` turns the selected root into a separately scanned topic group, so newly published replies get their own unread article numbers.

The `notifications` group uses the site's authenticated `/notifications.json` endpoint. It keeps only direct reply notifications (type 2), combines notifications by topic, and gives a topic a new Gnus article number when a new reply arrives. `M-x nndiscourse-open-notification` expands that topic and visits its newest notified floor. Authorize the site with `M-x discourse-auth-login` before scanning this group. Mentions and likes are excluded.

`M-x nndiscourse-compose-topic` asks for a forum URL and a category, then opens `message-mode` to write the subject and Markdown body. `C-c C-c` posts and subscribes to the created topic. Reply to a known article using normal Gnus followup, including from `gnus-thread-reader` if you use it. New topic composition looks up category names and IDs through `/categories.json`.

`gnus-thread-reader` is a **generic** continuous Gnus conversation view; this fork contains no Discourse-specific reading renderer. In a Gnus summary, `M-x gnus-thread-reader-open` works with this backend as with other backends. The native Gnus summary and article buffers work without that package.

## Authorization and persistence

The included `discourse-auth.el` implements the site's browser device authorization flow. It requests `read,write` by default, verifies the returned RSA OAEP payload and nonce, and keeps the User API Key per HTTPS site. The forum must support device authorization and permit the requested scopes.

The key lookup uses standard Emacs `auth-sources`. A matching authinfo entry has host set to the normalized HTTPS forum base, port `discourse-user-api`, login set to the API client ID, and password set to the API key. There is no hardcoded credential store. Newly authorized keys work for the current Emacs session. Set `discourse-auth-save-function` to a function of `(BASE CREDENTIAL)` if you want automatic persistence; `CREDENTIAL` is a plist with `:client` and `:key`. See `discourse-auth.el` for the function contract. Revocation happens in the forum account's application settings.

## Posting and safety

A reply uses `topic_id`, `reply_to_post_number`, and `raw`; a new topic uses `title`, `raw`, and `category`. Posting is restricted to the selected site's API endpoint. Message buffers support plain Markdown and reject attachments. The backend records a send fingerprint in a private local snapshot before dispatch. After an uncertain timeout or response, the draft remains and the same post is locked against blind resubmission. Check the website first, then run `M-x nndiscourse-clear-uncertain-sends` only if the post was not published. HTTP 4xx rejection unlocks retry after editing. The snapshot does not contain API keys.

The Gnus backend's snapshot lives under `gnus-directory/nndiscourse-topics/` with private permissions. It atomically replaces a topic only after all post IDs have been retrieved and validated. Missing parent posts become placeholders. A refresh never resets Gnus read marks.

## Gnus search

Gnus search is available with `G g` in the Group buffer. The search engine
calls the site's native `/search.json` for the selected `latest` or category
groups. Matching posts are kept in an internal search group, so historical
results do not enter a subscribed inbox. Category group searches are scoped
to that category.

## Test

```sh
emacs --batch --eval '(package-activate-all)' -L . \
  --eval '(setq discourse-auth-openssl-program "openssl")' \
  -l tests/discourse-auth-test.el -l tests/nndiscourse-test.el \
  -f ert-run-tests-batch-and-exit
```

Tests use synthetic credentials and mocked post responses. Read-only validation fetched 30 topic roots from each of Emacs China's Org-mode and Emacs-general categories; no live post was published.

GPL-3.0-or-later. The full history and license attribution of the original repository are preserved.
