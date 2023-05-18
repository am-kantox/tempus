defmodule Tempus.Sigils do
  @moduledoc "Handy sigils to instantiate `Tempus.Slot` and `Tempus.Slots`"

  alias Tempus.Slot

  defmodule NilParser do
    @moduledoc false

    def from_iso8601("nil"), do: {:ok, nil}
    def from_iso8601("∞"), do: {:ok, nil}
    def from_iso8601(_other), do: {:error, :not_a_nil}
  end

  @doc ~S"""
  Handles the sigil `~I` for `Tempus.Slot`.
  It returns a slot without interpolations and without escape
  characters, except for the escaping of the closing sigil character
  itself.

  _Allowed separators:_ `|`, `→`, `->`, `..` (might be surrounded by spaces)

  _Allowed modifiers:_

  - _none_ — expects two instances of `DateTime`
  - _single letter_, one of `d`, `t`, `u` — both values are expected to be of that type
  - _any combination_ of two letters `d`, `t`, `u` — the values are treated respectively
  - `g` — the value types are to be guessed

  ## Examples
      iex> import Tempus.Sigils
      iex> ~I(2021-03-30T06:35:40Z|2021-03-30T06:36:00Z)
      %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z]}
      iex> ~I(2021-03-30 → 2021-03-31)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-03-31 23:59:59.999999Z]}
      iex> ~I(2021-03-30)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-03-30 23:59:59.999999Z]}
      iex> ~I(2021-03-30 06:35:40Z .. 2021-03-30 06:36:00Z)g
      %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z]}

  """
  defmacro sigil_I({:<<>>, _, [binary]}, ~c"g") when is_binary(binary) do
    from_iso = List.duplicate(&Tempus.guess/1, 2)
    do_ast(binary, from_iso, __CALLER__)
  end

  defmacro sigil_I({:<<>>, _, [binary]}, modifiers) when is_binary(binary) do
    by_mod = fn
      ?d -> Date
      ?t -> Time
      ?u -> DateTime
      ?n -> NilParser
    end

    from_iso =
      modifiers
      |> case do
        [] -> [DateTime, DateTime]
        [ft] when ft in ~c"dtun" -> List.duplicate(by_mod.(ft), 2)
        [f, t] = mods when f in ~c"dtun" and t in ~c"dtun" -> Enum.map(mods, by_mod)
      end
      |> Enum.map(&Function.capture(&1, :from_iso8601, 1))

    do_ast(binary, from_iso, __CALLER__)
  end

  @doc """
  Parses the sigil-like binary representation of a `Tempus.Slot`.

  ## Examples

      iex> Tempus.Sigils.parse("2021-03-30T06:35:40Z|2021-03-30T06:36:00Z")
      {:ok, %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z]}}
  """
  @spec parse(input :: binary()) :: {:ok, Slot.t()} | {:error, any()}
  def parse(input) do
    result =
      input
      |> do_split()
      |> Enum.reduce_while([], fn s, acc ->
        s
        |> Tempus.guess()
        |> case do
          {:ok, slot} -> {:cont, [Slot.join([Slot.wrap(slot) | acc])]}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    with [%Slot{} = slot] <- result, do: {:ok, slot}
  end

  defp do_ast(input, from_iso, caller) do
    result =
      input
      |> do_split()
      |> Enum.zip(from_iso)
      |> Enum.map(fn {value, mapper} ->
        case mapper.(value) do
          {:ok, nil} ->
            nil

          {:ok, result} ->
            Slot.wrap(result)

          {:ok, result, _} ->
            Slot.wrap(result)

          {:error, error} ->
            raise CompileError,
              file: caller.file,
              line: caller.line,
              description: "`~I` sigil input is malformed, error: " <> inspect(error)
        end
      end)
      |> case do
        [nil, %Slot{to: to}] -> %Slot{from: nil, to: to}
        [%Slot{from: from}, nil] -> %Slot{from: from, to: nil}
        slots -> Slot.join(slots)
      end
      |> Macro.escape()

    quote(generated: true, location: :keep, do: unquote(result))
  end

  defp do_split(input), do: Regex.split(~r{\s*(?:\||→|\->|\.\.)\s*}, input)
end
