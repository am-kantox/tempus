defmodule Tempus.Sigils do
  @moduledoc "Handy sigils to instantiate `Tempus.Slot` and `Tempus.Slots`"

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
    by_mod = fn
      ?d -> Date
      ?t -> Time
      ?u -> DateTime
    end

    from_iso =
      modifiers
      |> case do
        [] -> [DateTime, DateTime]
        [ft] when ft in 'dtu' -> List.duplicate(by_mod.(ft), 2)
        [f, t] = mods when f in 'dtu' and t in 'dtu' -> Enum.map(mods, by_mod)
      end
      |> Enum.map(&Function.capture(&1, :from_iso8601, 1))

    result =
      binary
      |> String.split("|")
      |> Enum.zip(from_iso)
      |> Enum.map(fn {value, mapper} ->
        case mapper.(value) do
          {:ok, result} ->
            Slot.wrap(result)

          {:ok, result, _} ->
            Slot.wrap(result)

          {:error, error} ->
            raise CompileError,
              file: __CALLER__.file,
              line: __CALLER__.line,
              description: inspect(error)
        end
      end)
      |> Slot.join()
      |> Macro.escape()

    quote(generated: true, location: :keep, do: unquote(result))
  end
end
