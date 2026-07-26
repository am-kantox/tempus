# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

Tempus is an Elixir library for handling time periods (slots), including business days, holidays, and schedule management. It provides fast implementations for time slot operations like merging, overlapping detection, and schedule arithmetic.

## Common Commands

### Development
```bash
# Get dependencies
mix deps.get

# Compile the project
mix compile

# Run tests
mix test

# Run a specific test file
mix test test/tempus_test.exs

# Run a specific test by line number
mix test test/tempus_test.exs:42

# Format code
mix format

# Run Credo for static analysis
mix credo

# Run Credo in strict mode
mix credo --strict

# Run Dialyzer for type checking
mix dialyzer

# Run all quality checks (format, credo, dialyzer)
mix quality

# Run quality checks for CI (includes format check)
mix quality.ci

# Run test coverage
mix coveralls

# Generate documentation
mix docs
```

### CI Environment
```bash
# The project uses MIX_ENV=ci for some tasks
MIX_ENV=ci mix quality.ci
```

## Code Architecture

### Core Concepts

**Mathematical Foundation**: The library is built on group theory. Slots form an Abelian group with union as the binary operation. Understanding this mathematical background (see `stuff/background.md`) is crucial for working with the core implementation.

**Module Hierarchy**:
- `Tempus` - Main API module providing high-level functions for slot manipulation
- `Tempus.Slot` - Individual time slot struct with `from` and `to` DateTime fields
- `Tempus.Slots` - Collection of slots with pluggable backend implementations
- `Tempus.Slots.List` - Default implementation using ordered lists
- `Tempus.Slots.Stream` - Stream-based implementation for lazy evaluation
- `Tempus.Slots.Group` - Protocol for slot collection implementations
- `Tempus.Guards` - Compile-time guards for slot operations
- `Tempus.Sigils` - `~I` sigil for easy slot creation
- `Tempus.Crontab` - Cron expression parsing and handling

### Key Design Patterns

1. **Protocol-Based Backends**: `Tempus.Slots` uses the `Group` protocol to support different storage backends (List, Stream, AVLTree). When adding features, ensure they work with the protocol interface.

2. **Guard-Heavy Code**: The codebase extensively uses Elixir guards for performance. See `Tempus.Guards` for slot-related guards like `is_slot_covered/2`, `is_datetime_covered/2`, etc.

3. **Slot Normalization**: When slots are added or merged, overlapping slots are automatically joined. The library maintains the invariant that slots in a collection never overlap.

4. **Identity Element**: `Slot.id()` or `%Slot{from: nil, to: nil}` represents the identity element (void slot). `nil` in slot boundaries represents infinity.

5. **Origin Wrapping**: Most functions accept flexible input types (Date, DateTime, Slot) via `Slot.wrap/1` which normalizes them to Slot structs.

### Module Dependencies

- `telemetria` - Optional telemetry support (controlled by `:telemetria?` config)
- `formulae` - Used for formula parsing
- `avl_tree` - Optional backend for efficient slot storage
- `tzdata` - Timezone support (dev/test only)

### Testing Strategy

Tests are co-located with their modules in `test/` directory. Each major module has a corresponding test file:
- `test/tempus_test.exs` - Main API tests
- `test/tempus_slots_test.exs` - Slots collection tests
- `test/tempus_guards_test.exs` - Guards tests
- `test/tempus_sigils_test.exs` - Sigils tests

Doctest examples are heavily used throughout the codebase - when modifying functions, ensure doctests remain valid.

### Configuration

The default slots implementation can be changed via application config:
```elixir
config :tempus, :implementation, Tempus.Slots.List  # default
```

Telemetria support is opt-in:
```elixir
config :tempus, :telemetria?, true
```

## Development Notes

- **Elixir Version**: Requires Elixir ~> 1.14
- **OTP Support**: Tested on OTP 25-27
- **Breaking Changes**: Version 0.16.0 deprecated `is_covered/2` - use appropriate alternatives
- **DST Handling**: `recurrent/3` handles Daylight Saving Time transitions
- **Calendar Support**: Experimental support for different calendar systems

## GitHub Actions

- **Test Workflow**: Runs on push/PR with coverage reporting to Coveralls
- **Dialyzer Workflow**: Scheduled daily at 1:30 AM UTC, runs `mix quality.ci`
