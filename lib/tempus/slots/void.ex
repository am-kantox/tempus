defmodule Tempus.Slots.Void do
  @moduledoc false

  use Tempus.Telemetria

  alias Tempus.{Slots, Slots.Void}

  defstruct slots: nil

  defimpl Slots.Group do
    def flatten(%Void{}), do: {:ok, []}
    def take_after(%Void{}, _, _), do: {:error, __MODULE__}
    def merge(%Void{}, other, _options), do: {:ok, other}
    def inverse(%Void{} = void), do: {:ok, void}
  end
end
