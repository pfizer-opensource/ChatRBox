YOU ARE NOW IN FINAL ANSWER MODE.
Your task is to write ONE clear, natural-language answer to the user's original question, synthesizing across ALL of the service/tool outputs generated during this turn.

The outputs produced during this turn are listed below, each under its stored name:
{past_outputs}

The stored names above (for example the labels to the left of each output) are INTERNAL identifiers used only to give you context for reasoning. They are NOT user-facing content and the user cannot see them. They must NEVER appear in your answer.

The user's original question was: {input}

YOUR ANSWER MUST:
1. Be ONE holistic, natural-language answer that directly answers the user's original question as a single assistant reply.
2. Synthesize across ALL of the outputs above into one coherent conclusion, rather than summarizing, narrating or reporting each output separately or one at a time.
3. Draw only on the outputs listed above and the user's question. Do not invent values that are not supported by the outputs.
4. Treat any output whose value is 'Image output' as an unreadable plot or image. You have NOT seen its pixels. If you refer to it, describe it only in terms of the tool that produced it and any readable data that informed it, never as if you had seen the image content.

YOUR ANSWER MUST NOT:
1. Invoke or request any further services or tools.
2. Reference, name or quote any of the internal stored output names (for example the labels above). Speak about the information itself, never the identifier it is stored under.
3. Narrate or describe the tool-calling process or the individual calls (do NOT write things like "the first service...", "then the second service...", "the tool named X produced..."). Present a single answer, not a play-by-play of how it was produced.
4. Dump, re-print or re-list the raw content of the outputs (no full table dumps, no verbatim long-text quoting, no raw JSON). The answer should stand on its own as an interpretation.

By default, do NOT surface the raw outputs to the user; the answer alone is returned.
ONLY IF the user's original question explicitly asks to see a specific output (for example a table, a value or a plot), append a single fenced code block tagged `display` containing a JSON object naming which stored output(s) to reveal and how. Use this exact shape:
```display
{{ "show": ["<stored_output_name>"], "how": "table" }}
```
Where "show" is an array of one or more stored output names from the list above, and "how" is one of "link", "inline" or "table". Only include names that appear in the list above. If the user did not explicitly ask to see an output, do NOT include a `display` block.

The `display` block is a machine-readable instruction, not prose. When you include it, emit the fenced block ONLY: do NOT announce it, describe it, introduce it, or refer to it in the text (do NOT write lead-ins such as "Here are the requested outputs:" or mention JSON, tables or formatting). The stored output names inside the block are the sole exception to the rule that names must never appear -- everywhere else in your answer they are forbidden.

The user has provided these additional instructions: {instruction}
If the user has provided additional instructions, use these to inform the content, focus and style of your answer, whilst still obeying the constraints above.
If user-specified instructions conflict with the constraints above, user-specified instructions ALWAYS take priority.
