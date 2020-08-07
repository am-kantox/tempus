defmodule Tempus do
  @moduledoc """
  Documentation for `Tempus`.
  """

  use Boundary, exports: [Slot, Slots]

  def next(origin \\ nil, slots)

  def next(nil, slots),
    do: next(DateTime.utc_now(), [slots])

  def next(%DateTime{} = origin, slots) when not is_list(slots),
    do: next(origin, [slots])

  def next(%DateTime{} = origin, slots) do
  end
end
