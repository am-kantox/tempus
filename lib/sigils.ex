defmodule Tempus.Sigils do
  @moduledoc "Handy sigils to instantiate `Tempus.Slot` and `Tempus.Slots`"

  alias Tempus.Slot

  defmodule NilParser do
    @moduledoc false

    def from_iso8601(nil), do: {:ok, nil}
    def from_iso8601(""), do: {:ok, nil}
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
  - _any combination_ of two letters `d`, `t`, `u` — the values are treated respectively
  - `g` — the value types are to be guessed

  ## Examples
      iex> import Tempus.Sigils
      iex> ~I(2021-03-30T06:35:40Z|2021-03-30T06:36:00Z)
      %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z], from_open: false, to_open: true}
      iex> ~I(2021-03-30 → 2021-03-31)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-04-01 00:00:00.000000Z], from_open: false, to_open: true}
      iex> ~I(2021-03-30)d
      %Tempus.Slot{from: ~U[2021-03-30 00:00:00.000000Z], to: ~U[2021-03-31 00:00:00.000000Z], from_open: false, to_open: true}
      iex> ~I(2021-03-30 06:35:40Z .. 2021-03-30 06:36:00Z)g
      %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z], from_open: false, to_open: true}
      iex> ~I(|2021-03-30 06:36:00Z)g
      %Tempus.Slot{from: nil, to: ~U[2021-03-30 06:36:00Z], from_open: true, to_open: true}
      iex> ~I(2021-03-30 06:36:00Z|)g
      %Tempus.Slot{from: ~U[2021-03-30 06:36:00Z], to: nil, from_open: false, to_open: true}
      iex> Code.compile_quoted(quote do: ~I(foo|bar)g)
      ** (CompileError) nofile: `~I` sigil input is malformed, error: :invalid_format
  """
  defmacro sigil_I({:<<>>, _, [binary]}, ~c"g") when is_binary(binary) do
    from_iso = List.duplicate(iso_mapper(DateTime), 2)
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
        [] -> List.duplicate(DateTime, 2)
        [ft] when ft in ~c"dtun" -> List.duplicate(by_mod.(ft), 2)
        [f, t] = mods when f in ~c"dtun" and t in ~c"dtun" -> Enum.map(mods, by_mod)
      end
      |> Enum.map(&iso_mapper/1)

    do_ast(binary, from_iso, __CALLER__)
  end

  defp iso_mapper(mod) do
    fn val ->
      val = String.replace(val, " ", "T")

      case mod.from_iso8601(val) do
        {:ok, result, _} -> {:ok, result}
        {:ok, result} -> {:ok, result}
        {:error, _} -> Tempus.guess(val)
      end
    end
  end

  @doc """
  Parses the sigil-like binary representation of a `Tempus.Slot`.

  ## Examples

      iex> Tempus.Sigils.parse("2021-03-30T06:35:40Z|2021-03-30T06:36:00Z")
      {:ok, %Tempus.Slot{from: ~U[2021-03-30 06:35:40Z], to: ~U[2021-03-30 06:36:00Z], from_open: false, to_open: true}}
      iex> Tempus.Sigils.parse("foo|bar")
      {:error, :invalid_format}
  """
  @spec parse(input :: binary()) :: {:ok, Slot.t()} | {:error, any()}
  def parse(input) do
    {from_open, to_open, clean_input} = extract_bounds(input)

    parts = do_split(clean_input)

    case Enum.map(parts, &Tempus.guess/1) do
      [{:ok, s1}, {:ok, s2}] ->
        s1 = Slot.wrap(s1)
        s2 = Slot.wrap(s2)

        slot = %Slot{
          from: s1.from,
          to: s2.to || s2.from,
          from_open: if(is_nil(from_open), do: false, else: from_open),
          to_open: if(is_nil(to_open), do: true, else: to_open)
        }

        {:ok, slot}

      [{:ok, s1}] ->
        %Slot{} = s1 = Slot.wrap(s1)

        slot = %Slot{
          s1
          | from_open: if(is_nil(from_open), do: s1.from_open, else: from_open),
            to_open: if(is_nil(to_open), do: s1.to_open, else: to_open)
        }

        {:ok, slot}

      _ ->
        {:error, :invalid_format}
    end
  end

  defp do_ast(input, from_iso, caller) do
    {from_open, to_open, clean_input} = extract_bounds(input)

    result =
      clean_input
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
        [nil, %Slot{to: to, to_open: default_to_op}] ->
          %Slot{
            from: nil,
            to: to,
            from_open: true,
            to_open: if(is_nil(to_open), do: default_to_op, else: to_open)
          }

        [%Slot{from: from, from_open: default_from_op}, nil] ->
          %Slot{
            from: from,
            to: nil,
            from_open: if(is_nil(from_open), do: default_from_op, else: from_open),
            to_open: true
          }

        [%Slot{from: f}, %Slot{to: t_to, from: t_from}] ->
          to = if is_nil(t_to), do: t_from, else: t_to

          %Slot{
            from: f,
            to: to,
            from_open:
              if(is_nil(f), do: true, else: if(is_nil(from_open), do: false, else: from_open)),
            to_open: if(is_nil(to), do: true, else: if(is_nil(to_open), do: true, else: to_open))
          }

        [%Slot{} = s] ->
          %Slot{
            s
            | from_open:
                if(is_nil(s.from),
                  do: true,
                  else: if(is_nil(from_open), do: s.from_open, else: from_open)
                ),
              to_open:
                if(is_nil(s.to),
                  do: true,
                  else: if(is_nil(to_open), do: s.to_open, else: to_open)
                )
          }
      end
      |> Macro.escape()

    quote(generated: true, location: :keep, do: unquote(result))
  end

  defp extract_bounds(input) do
    input = String.trim(input)
    len = String.length(input)

    {from_open, input} =
      cond do
        String.starts_with?(input, "]") or String.starts_with?(input, "(") ->
          {true, String.slice(input, 1, len - 1)}

        String.starts_with?(input, "[") ->
          {false, String.slice(input, 1, len - 1)}

        true ->
          {nil, input}
      end

    len = String.length(input)

    {to_open, input} =
      cond do
        String.ends_with?(input, "[") or String.ends_with?(input, ")") ->
          {true, String.slice(input, 0, len - 1)}

        String.ends_with?(input, "]") ->
          {false, String.slice(input, 0, len - 1)}

        true ->
          {nil, input}
      end

    {from_open, to_open, String.trim(input)}
  end

  defp do_split(input), do: Regex.split(~r{\s*(?:\||→|\->|\.\.)\s*}, input)
end
