defmodule Tempus.Slots.Behaviour do
  @moduledoc """
  The bunch of functions expected to be implemented by `Tempus.Slots` implementations.
  """

  @doc """
  Creates a new instance, which is also usually an identity element
  """
  @callback new :: Tempus.Slots.Group.t()
end
