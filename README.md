# ![Tempus](https://raw.githubusercontent.com/am-kantox/tempus/master/stuff/tempus-48x48.png) Tempus    [![Kantox ❤ OSS](https://img.shields.io/badge/❤-kantox_oss-informational.svg)](https://kantox.com/)  ![Test](https://github.com/am-kantox/tempus/workflows/Test/badge.svg)  ![Dialyzer](https://github.com/am-kantox/tempus/workflows/Dialyzer/badge.svg)

**Easy handling of time periods aka slots, like business days, holidays, etc.**

## Installation

```elixir
def deps do
  [
    {:tempus, "~> 0.1"}
  ]
end
```

## Changelog
- **`0.7.1`** — treat infinite slot as special not-covering anything
- **`0.7.0`** — `merge/2` is 6× faster, `Slot.neighbour?/2`, `Tempus.slice/4`, `Tempus.drop_while/2`, `Tempus.take_while/2`
- **`0.6.0`** — compatibility with _Elixir v1.14_ (no greek in variables names) meh
- **`0.5.0`** — stricter `:telemetria` support
- **`0.4.2`** — make `:telemetria` dependency fully optional
- **`0.4.0`** — `~I` sigil to ease slots creation, wrap date/times, fancy inspect
- **`0.3.0`** — drastical performance improvements, benchmarks
- **`0.2.4`** — fixed bug when finding next free/busy for empty slots
- **`0.2.3`** — correctly handle empty slots in `next_busy/2`/`next_free/2`
- **`0.2.2`** — `Slot.shift_tz/3`
- **`0.2.1`** — accept function as well as stream in `Slots.merge/2`
- **`0.2.0`** — many improvements, `Tempus.add/2` similar to `DateTime.add/4` but considering slots

## [Documentation](https://hexdocs.pm/tempus)
