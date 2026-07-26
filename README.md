# ![Tempus](https://raw.githubusercontent.com/am-kantox/tempus/master/stuff/tempus-48x48.png) Tempus    [![Kantox ❤ OSS](https://img.shields.io/badge/❤-kantox_oss-informational.svg)](https://kantox.com/)  ![Test](https://github.com/am-kantox/tempus/workflows/Test/badge.svg)  ![Dialyzer](https://github.com/am-kantox/tempus/workflows/Dialyzer/badge.svg)  [![Coverage Status](https://coveralls.io/repos/github/am-kantox/tempus/badge.svg?branch=master)](https://coveralls.io/github/am-kantox/tempus?branch=master)

**Easy handling of time periods aka slots, like business days, holidays, schedules, and cron-like recurring slots.**

---

## Why `Estructura`? Elevating Code Quality

`Tempus` and related libraries in the ecosystem rely heavily on [`Estructura`](https://hexdocs.pm/estructura), an advanced data-structuring and domain-modelling framework for Elixir.

Building complex domain libraries (especially time-arithmetic and schedule processing engines) requires strict data guarantees, type-safety, and minimal boilerplate. `Estructura` dramatically improves code quality across several crucial dimensions:

### 1. Strict Validation & Type Coercion at Boundaries
Instead of manually writing defensive functions or risking invalid internal state, `Estructura` allows defining strict field schemas with type coercion and validation contracts. Inputs are normalized at construction time, ensuring data structures are guaranteed valid throughout the pipeline.

```elixir
defmodule BusinessHours do
  use Estructura

  defstruct [
    open_time: {Time, :from_iso8601!},
    close_time: {Time, :from_iso8601!}
  ]

  # Enforces runtime contracts and validates constraints
  def validate(%__MODULE__{open_time: o, close_time: c}) do
    if Time.compare(c, o) == :gt, do: :ok, else: {:error, :invalid_hours}
  end
end

# Coerces ISO8601 string inputs automatically into %Time{} structs
{:ok, hours} = BusinessHours.new(open_time: "09:00:00", close_time: "17:00:00")
```

### 2. Guard & Pattern Matching Primitives
`Estructura` generates compile-time guard helpers and pattern-matching primitives (`is_...`), avoiding ad-hoc map key checks or defensive `is_map/1` guards across module boundaries.

### 3. Ergonomic Immutability & Nested Access (Access Protocol & Lenses)
Manipulating deeply nested data structures in idiomatic Elixir can become verbose. `Estructura` generates full `Access` protocol implementations and lens transformers (`get_in`, `put_in`, `update_in`), making complex nested updates concise and type-safe.

```elixir
# Update nested values safely using generated Access lenses
update_in(schedule, [:business_hours, :open_time], &Time.add(&1, 3600))
```

### 4. Automated Property-Based Testing Integration
`Estructura` integrates directly with `StreamData`, automatically deriving generators for data structures. This allows instant property-based testing of domain models without manually constructing complex test generators.

```elixir
# Automatically generate random valid structs for property tests
property "slots are always valid" do
  check all slot <- BusinessHours.generator() do
    assert Time.compare(slot.close_time, slot.open_time) == :gt
  end
end
```

---

## Key Concepts in Tempus

`Tempus` models time periods using **half-open intervals** $[from, to)$ (`from_open: false`, `to_open: true` by default):

- **Half-Open Intervals**: A standard slot $[00:00:00, 00:00:00\text{ next day})$ covers the whole day up to, but not including, midnight of the next day. This simplifies slot joining, adjacency checks, and avoids 1-microsecond arithmetic hacks.
- **Infinite / Unbound Limits**: A `nil` bound represents infinity ($-\infty$ or $+\infty$) and is always marked as **open** (`true`).
- **`~I` Sigil**: Concise sigil syntax for constructing slots with custom boundary openness:
  - `~I[2023-04-10 10:00:00Z → 2023-04-10 12:00:00Z)` — closed `from`, open `to`.
  - `~I(2023-04-10 10:00:00Z → 2023-04-10 12:00:00Z]` — open `from`, closed `to`.

---

## Installation

Add `tempus` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:tempus, "~> 0.16"}
  ]
end
```

---

## Usage Examples

### 1. Creating Slots with `~I` Sigil
```elixir
import Tempus.Sigils

# Whole day slot [2023-04-10 00:00:00Z -> 2023-04-11 00:00:00Z)
day_slot = ~I(2023-04-10)d

# Explicit datetime slot
slot = ~I[2023-04-10 09:00:00Z → 2023-04-10 17:00:00Z)
```

### 2. Schedule Arithmetic & Busy/Free Time
```elixir
alias Tempus.{Slot, Slots}
import Tempus.Sigils

# Define holiday and weekend slots
holidays = [~D[2020-08-06], ~D[2020-08-13]] |> Tempus.slots()
weekends = [~D[2020-08-08], ~D[2020-08-09]] |> Tempus.slots()

schedule = Slots.merge(holidays, weekends)

# Calculate 5 business days ahead starting from Aug 5th
business_days = Tempus.days_ahead(schedule, ~D[2020-08-05], 5)
```

---

## Changelog
- **`0.16.0`** — [:books:] half-open slot interval refactoring (`[from, to)`), `nil` open infinity bounds, `v1.18.0`, deprecation of `is_covered/2`
- **`0.15.0`** — [:tada:] `Tempus.parse_cron/2` and low-level `Tempus.Crontab`
- **`0.14.1`** — [:ant:] `recurrent/3` has now concerned about DST
- **`0.14.0`** — [:books:] refactoring of guard usage and docs update
- **`0.13.3`** — [:ant:] avoid crash on incorrect input of slots
- **`0.13.2`** — [:tada:] `Tempus.Slots.Stream.recurrent/3` to introduce cron-like streams
- **`0.13.0`** — [:tada:] different calendars experimental support
- **`0.12.1`** — [:tada:] timezones and more guards exported
- **`0.11.0`** — [:books:] better coverage
- **`0.10.2`** — [:ant:] `add/4` and tests for it: fixed
- **`0.10.1`** — [:ant:] `split/4` and tests for it: fixed
- **`0.10.0`** — complete rewrite of implementations, 3–10× faster, `Slots` form an Abelian group now

---

## [Documentation](https://hexdocs.pm/tempus)
