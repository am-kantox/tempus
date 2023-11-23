defmodule Tempus.Slots.Normalizers do
  @moduledoc false

  alias Tempus.{Slot, Slots}
  import Tempus.Guards, only: [is_coming_before: 2]

  @spec pop_jid([{:join, nil | boolean() | non_neg_integer()} | keyword()]) ::
          nil | non_neg_integer()
  def pop_jid(options) do
    case Keyword.get(options, :join, false) do
      true -> 1
      value when is_integer(value) and value > 0 -> value
      false -> nil
      nil -> nil
      other -> tap(nil, fn _ -> warning_jid(other) end)
    end
  end

  @spec to_locator(Slots.locator()) :: (Slot.t() -> boolean())
  def to_locator(slot, negate? \\ false)
  def to_locator(%Slot{} = slot, false), do: &is_coming_before(slot, &1)
  def to_locator(%Slot{} = slot, true), do: &is_coming_before(&1, slot)
  def to_locator(fun, false) when is_function(fun, 1), do: fun
  def to_locator(fun, true) when is_function(fun, 1), do: fn arg -> not fun.(arg) end

  defp warning_jid(other),
    do: IO.warn("`:join` argument must be boolean or integer, #{inspect(other)} given")
end
