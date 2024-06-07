global_settings = "~/.iex.exs"
if File.exists?(global_settings), do: Code.require_file(global_settings)

Application.put_env(:elixir, :ansi_enabled, true)

IEx.configure(
  inspect: [limit: :infinity],
  colors: [
    eval_result: [:cyan, :bright],
    eval_error: [:red, :bright],
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
  default_prompt: [
      :blue,
      "%prefix",
      :yellow,
      "|⌚|",
      :blue,
      "%counter",
      :yellow,
      " ▸",
      :reset
    ]
    |> IO.ANSI.format()
    |> IO.chardata_to_string()
)

import Tempus.{Guards, Guards.Calendar, Sigils}
alias Tempus.{Slot, Slots}

microseconds_in_week = 604_800_000_000

first_weekend = %Slot{from: ~U|2018-01-05 21:00:00.000000Z|, to: ~U|2018-01-08 08:59:59.999999Z|}

stream =
  Tempus.Slots.Stream.iterate(
    first_weekend,
    fn acc ->
      acc
      |> Tempus.Slot.shift(from: microseconds_in_week, to: microseconds_in_week)
      |> Tempus.Slot.shift_tz()
    end,
    return_as: :slots
  )

list = stream |> Enum.take(10) |> Enum.into(%Slots{})
