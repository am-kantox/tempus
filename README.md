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

- **`0.2.2`** — `Slot.shift_tz/3`
- **`0.2.1`** — accept function as well as stream in `Slots.merge/2`
- **`0.2.0`** — many improvements, `Tempus.add/2` similar to `DateTime.add/4` but considering slots

## [Documentation](https://hexdocs.pm/tempus)
