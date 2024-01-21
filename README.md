# ![Tempus](https://raw.githubusercontent.com/am-kantox/tempus/master/stuff/tempus-48x48.png) Tempus    [![Kantox ❤ OSS](https://img.shields.io/badge/❤-kantox_oss-informational.svg)](https://kantox.com/)  ![Test](https://github.com/am-kantox/tempus/workflows/Test/badge.svg)  ![Dialyzer](https://github.com/am-kantox/tempus/workflows/Dialyzer/badge.svg)  [![Coverage Status](https://coveralls.io/repos/github/am-kantox/tempus/badge.svg?branch=master)](https://coveralls.io/github/am-kantox/tempus?branch=master)

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
- **`0.13.0`** — [:tada:] different calendars experimental support
- **`0.12.1`** — [:tada:] timezones and more guards exported
- **`0.11.0`** — [:books:] better coverage
- **`0.10.2`** — [:ant:] `add/4` and tests for it: fixed
- **`0.10.1`** — [:ant:] `split/4` and tests for it: fixed
- **`0.10.0`** — complete rewrite of implementations, 3–10× faster, `Slots` form an Abelian group now
- **`0.9.1`** — `Tempus.Sigils.parse/1`
- **`0.9.0`** — `Tempus.Guards`, `Tempus.slot/{1,2}`, `Tempus.guess/1`, better sigils, prepared for `:tempus_sql`
- **`0.8.0`** — improve `Tempus.Sigils`, prepared for `:tempus_sql`
- **`0.7.3`** — `truncate: boolean() | non_neg_integer()` to truncate `Slots` inspection
- **`0.7.2`** — `Slots.merge/1`
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
