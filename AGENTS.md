# Agent Guidelines for vanilla-emacs

## Project Overview
This is a Japanese documentation project for transitioning from Doom Emacs to Vanilla Emacs. It contains comprehensive guides, configuration examples, and troubleshooting information written in Markdown.

## Build/Test Commands
- No build system or package manager detected
- No test framework found
- Files are standard Markdown documentation
- To preview: Use any Markdown viewer or `markdown` command if available

## Code Style Guidelines

### Language & Writing
- **Primary language**: Japanese with some English technical terms
- **Code blocks**: Use triple backticks with language identifiers (`elisp`, `bash`, `org`)
- **File encoding**: UTF-8

### Markdown Conventions
- Use `#` for chapter headings (第X章)
- Use `##` for major sections  
- Use `###` for subsections
- Use `####` for detailed topics
- Code examples in fenced code blocks with appropriate language tags
- Consistent use of Japanese punctuation (、。)

### Emacs Lisp Code Style
When including Emacs Lisp examples:
- Use 2-space indentation
- Include docstrings for functions
- Use `;;` for single-line comments
- Use `;;;` for section headers
- Follow standard Elisp naming conventions with dashes
- Use `use-package` format for configuration examples

### Configuration Examples
- Show complete, working configuration snippets
- Include explanatory text in Japanese before/after code blocks
- Use consistent variable naming and structure
- Include error handling where appropriate

### Documentation Structure
- Each major topic gets its own section
- Include practical examples and troubleshooting
- Use consistent formatting for commands, keybindings, and file paths
- Cross-reference related sections where helpful

## Content Guidelines
- Focus on practical, working solutions
- Include performance considerations (startup time, memory usage)
- Provide both beginner and advanced configurations
- Document common pitfalls and solutions
- Maintain consistency with established Emacs conventions