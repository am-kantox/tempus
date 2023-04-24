defmodule Tempus.Slots.Test do
  use ExUnit.Case, async: false
  doctest Tempus.Slots
  doctest Tempus.Slots.List
  doctest Tempus.Slots.Stream

  alias Tempus.{Slot, Slots}

  setup_all do
    _micros_in_hour = 3_600_000_000
    _micros_in_day = 86_400_000_000

    day_after_day_stream = Stream.iterate(~D|2023-01-01|, &Date.add(&1, 2))
    day_after_day_list = Enum.take(day_after_day_stream, 10)

    [day_after_day_stream: day_after_day_stream, day_after_day_list: day_after_day_list]
  end

  describe "List.Group" do
    setup ctx do
      [input: ctx.day_after_day_list, slots: Slots.new(:list, ctx.day_after_day_list)]
    end

    test "identity/1", %{slots: slots} do
      assert %Slots.List{slots: []} = Slots.identity(slots)
    end

    test "flatten/1", %{input: input, slots: slots} do
      assert Enum.map(input, &Slot.wrap/1) == Slots.flatten(slots)
    end
  end
end
