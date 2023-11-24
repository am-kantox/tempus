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

  @spec inverse_inequality(:gt | :eq | :lt | true | false) :: :gt | :eq | :lt | true | false
  defp inverse_inequality(:gt), do: :lt
  defp inverse_inequality(:lt), do: :gt
  defp inverse_inequality(:eq), do: :eq
  defp inverse_inequality(true), do: false
  defp inverse_inequality(false), do: true

  @spec to_locator(Slots.locator()) :: (Slot.t() -> :gt | :eq | :lt)
  def to_locator(slot, negate? \\ false)

  def to_locator(%Slot{} = slot, false) do
    fn other ->
      case {is_coming_before(slot, other), is_coming_before(other, slot)} do
        {false, false} -> :eq
        {true, false} -> :gt
        {false, true} -> :lt
      end
    end
  end

  def to_locator(%Slot{} = slot, true),
    do: fn other -> other |> to_locator(slot, false).() |> inverse_inequality() end

  def to_locator(fun, false) when is_function(fun, 1), do: fun

  def to_locator(fun, true) when is_function(fun, 1),
    do: fn other -> other |> fun.() |> inverse_inequality() end

  defp warning_jid(other),
    do: IO.warn("`:join` argument must be boolean or integer, #{inspect(other)} given")
end
