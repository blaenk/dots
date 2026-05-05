Store worktrees in ~/code/worktrees/<repo-name>/<worktree-name>

Never amend commits unless I specifically request it.

If you find that the approach I accepted doesn't actually work in practice, tell me.

## Local dev port allocation

Each project gets a 10-port block in the 3000s. Within a block, conventions are:

- `+0` — API / backend
- `+1` — web / frontend
- `+2..+9` — additional services as needed (worker dashboards, secondary UIs, tunnels)

Current allocations:

- 3000-3009: stream (api 3000, web 3001)
- 3010-3019: beam (api 3010, web 3011)

When starting a new project, claim the next free block and update this list. Edit `~/.local/share/chezmoi/dot_claude/CLAUDE.md` and run `chezmoi apply` so the change is captured.
