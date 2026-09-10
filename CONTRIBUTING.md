# Contributing to ChatRBox

## OOP Architecture

ChatRBox uses three object systems, each with a specific role. When extending the
package, follow the pattern that matches the kind of state or behavior you are adding.

### R6 — User-facing session state (`ChatRBox` class)

`ChatRBox` is an `R6` class because it needs mutable state across `$talk()` calls and is
the public interface users interact with directly. It wraps the underlying `ellmer` chat
object and a `ChatRBox_obj` `S7` object.

Add user-facing conversational or session features here.

### S7 — Validated internal state (`ChatRBox_obj` class)

`ChatRBox_obj` is an `S7` class because it holds typed, validated data (prompts, services,
environments, path/argument summaries). `S7`'s `validator` enforces correctness at
assignment time, analogous to `setValidity()` for `S4`.

Add new internal state that must be typed and validated as `S7` properties. Do NOT add it
directly to the `R6` class.

### S7 generics (`get_property`)

`get_property` is an `S7` generic used for dispatched property access on `ChatRBox_obj`.
Only add new generics when dispatch on the `S7` class is genuinely needed.

### Plain functions

All helpers (API client builders, prompt loaders, error formatters, image handlers)
should be plain R functions. Internal helpers should use `@noRd` and omit `@export` so
they remain usable within the package namespace without widening the public API.

## Adding a New Structured Output Language

`code_extract()` and `code_remove()` support Markdown fenced code blocks for any
language tag. The fence-detection regex is built dynamically and imposes no constraint
on the tag itself; the only gate is the `.SUPPORTED_CODE_LANGUAGES` vector. To add
support for a new format:

1. Add the tag to `.SUPPORTED_CODE_LANGUAGES` in `R/generate_ai_response.R`.
2. Add a parsing branch in `llm_api_result()` to handle the new format after
   `code_extract()` returns (it currently assumes JSON via `jsonlite::fromJSON()`).
3. Update the system prompt template in `inst/prompt/prompt.md` to instruct the LLM to
   use the new format when appropriate.
4. Add a unit test in `tests/testthat/test_generate_ai_response.R` covering the new
   language tag.