defmodule Tempus.Sigils.Test do
  use ExUnit.Case, async: false
  doctest Tempus.Sigils

  alias Tempus.Slot
  import Tempus.Sigils
  import ExUnit.CaptureIO

  test "sigils" do
    assert ~I[2021-03-30]d == %Slot{
             from: ~U[2021-03-30 00:00:00.000000Z],
             to: ~U[2021-03-30 23:59:59.999999Z]
           }

    assert ~I[07:23:06]t == %Slot{
             from: ~U[2021-03-30 07:23:06Z],
             to: ~U[2021-03-30 07:23:06Z]
           }
  end

  test "fancy inspect" do
    assert capture_io(fn ->
             IO.inspect(~I[2021-03-30]d, custom_options: [fancy_inspect: true])
           end) == "⌚<2021-03-30T00:00:00.000000Z → 2021-03-30T23:59:59.999999Z>\n"
  end
end
