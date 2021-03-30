defmodule Tempus.Sigils do
  @moduledoc "Handy sigils to instantiate `Tempus.Slot`"

  alias Tempus.Slot

  @doc ~S"""
  Handles the sigil `~I` for `Tempus.Slot`.
  It returns a slot without interpolations and without escape
  characters, except for the escaping of the closing sigil character
  itself.

  ## Examples
      iex> import Tempus.Sigils
      iex> ~I(2021-03-30T06:35:40Z|2021-03-30T06:36:00Z)
      %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z]}
      iex> ~I(2021-03-30|2021-03-31)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-03-31 23:59:59.999999Z]}
      iex> ~I(2021-03-30)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-03-30 23:59:59.999999Z]}

  """
  defmacro sigil_I({:<<>>, _, [binary]}, modifiers) when is_binary(binary) do
    module =
      case modifiers do
        [?d] -> Date
        [?t] -> Time
        [] -> DateTime
      end

    from_iso = &(&1 |> module.from_iso8601() |> elem(1) |> Slot.wrap())

    result =
      binary
      |> String.split("|")
      |> case do
        [part] -> from_iso.(part)
        [_, _] = interval -> interval |> Enum.map(from_iso) |> Slot.join()
      end
      |> Macro.escape()

    quote(generated: true, location: :keep, do: unquote(result))
  end
end
