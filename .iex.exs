global_settings = "~/.iex.exs"
if File.exists?(global_settings), do: Code.require_file(global_settings)

Application.put_env(:elixir, :ansi_enabled, true)

IEx.configure(
  inspect: [limit: :infinity],
  colors: [
    eval_result: [:cyan, :bright],
    eval_error: [[:red, :bright, "\n▶▶▶\n"]],
    eval_info: [:yellow, :bright],
    syntax_colors: [
      number: :red,
      atom: :blue,
      string: :green,
      boolean: :magenta,
      nil: :magenta,
      list: :white
    ]
  ],
  default_prompt:
    [
      # cursor ⇒ column 1
      "\e[G",
      :blue,
      "%prefix",
      :blue,
      "|⌚|",
      :yellow,
      "%counter",
      " ",
      :blue,
      "▶",
      :reset
    ]
    |> IO.ANSI.format()
    |> IO.chardata_to_string()
)

import Tempus.{Guards, Sigils}
alias Tempus.{Slot, Slots}

microseconds_in_week = 604_800_000_000

first_weekend = %Slot{from: ~U|2018-01-05 21:00:00Z|, to: ~U|2018-01-08 08:59:59Z|}

weekends =
  %Tempus.Slots{slots: Tempus.Slots.Stream.iterate(
    first_weekend,
    fn acc ->
      acc
      |> Tempus.Slot.shift(from: microseconds_in_week, to: microseconds_in_week)
      |> Tempus.Slot.shift_tz()
    end
  )}

slots =
  [~D|2018-01-01|, ~D|2022-12-31|]
  |> Enum.into(%Slots{})
  |> Slots.merge(weekends)
