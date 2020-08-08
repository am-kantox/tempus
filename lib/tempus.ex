defmodule Tempus do
  @moduledoc """
  Documentation for `Tempus`.
  """

  use Boundary, exports: [Slot, Slots]

  alias Tempus.{Slot, Slots}

  @spec next_busy(Slots.t(), [{:origin, Slot.origin()} | {:count, pos_integer()}]) ::
          Slot.t() | nil | no_return
  @doc """
  Returns the next slot from the slots passed as a first argument,
    that immediately follows `origin`. IOf slots are overlapped, the overlapped
    one gets returned.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-08 23:00:00Z|, to: ~U|2020-08-09 12:00:00Z|})
      #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-07 12:00:00Z|}, count: 2) |> hd()
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-08 12:00:00Z|})
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-10 12:00:00Z|})
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-07|)
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-08|)
      #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-11 11:00:00Z|, to: ~U|2020-08-11 12:00:00Z|})
      nil

  """
  def next_busy(slots, opts \\ [])

  def next_busy(%Slots{} = slots, opts) do
    origin = Keyword.get(opts, :origin, DateTime.utc_now())

    case Keyword.get(opts, :count) do
      nil -> slots |> do_next_busy(origin, 1) |> List.first()
      i when is_integer(i) and i > 0 -> do_next_busy(slots, origin, i)
      other -> raise Tempus.ArgumentError, expected: Integer, passed: other
    end
  end

  def next_busy(%Slot{} = slot, opts),
    do: next_busy(Slots.add(%Slots{}, slot), opts)

  @spec do_next_busy(Slots.t(), Slot.origin(), pos_integer()) :: Slot.t() | nil | no_return
  defp do_next_busy(%Slots{} = slots, origin, count) do
    origin = Slot.wrap(origin)

    slots
    |> Enum.drop_while(fn
      %Slot{} = slot -> Slot.strict_compare(slot, origin) == :lt
      other -> raise Tempus.ArgumentError, expected: Tempus.Slot, passed: other
    end)
    |> Enum.take(count)
  end
end
