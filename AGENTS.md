# AGENTS.md

## Build/Lint/Test Commands

This is a dotfiles repository - no traditional build/test commands.

## Code Style Guidelines

### Python (scripts/)
- Use type hints: `def create_link(entry: os.DirEntry):`
- Import standard library first, then third-party
- Use pathlib for file operations
- Follow PEP 8 line length (120 chars in Emacs config)
- Use f-strings for string formatting

### Shell Configuration
- Fish: Use functions for complex logic, vi-mode keybindings
- Bash: Source exports first, then aliases
- Use `command -v` for command existence checks
- Follow XDG Base Directory conventions

### Configuration Files
- Use XDG paths: `~/.config/<tool>/`
- Keep tool-specific configs in their own directories
- Use semantic versioning for package managers where applicable

### Git Conventions
- Main branch: `main`
- Simple commit messages: "Update <tool> configuration"
- Use gitleaks for security scanning

### General
- No trailing whitespace
- Use semantic, descriptive variable names
- Prefer absolute paths in scripts
- Include error handling for file operations
