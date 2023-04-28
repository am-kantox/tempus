defprotocol Tempus.Slots.Group do
  @moduledoc """
  The protocol to implement for the ordered collection of slots.
  """

  alias Tempus.Slot

  @doc """
  The function returning the identity element, which can be merged into
  each and every implementation without changing it.
  """
  @spec identity(t()) :: t()
  def identity(slots)

  @doc """
  Flattens the implementation, returning the list back.
  """
  @spec flatten(t(), keyword()) :: [Slot.t()]
  def flatten(slots, options \\ [])

  @doc """
  Adds a single `t:Slot.t()` instance to this `t:Slots.t(t())`
    implementation. If `merge/3` implementation returns `{:error, __MODULE__}`,
    this function would be used to add elements one by one through `reduce/3`.
  """
  @spec add(t(), Slot.origin(), keyword()) :: t()
  def add(slots, slot, options \\ [])

  @doc """
  Efficient implementation of merging slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec merge(t(), [Slot.t()] | t(), keyword()) :: {:ok, t()} | {:error, module()}
  def merge(slots, other, options \\ [])

  @doc """
  Efficient implementation of splitting slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec split(t(), Slots.locator(), keyword()) :: {:ok, t(), t()} | {:error, module()}
  def split(slots, locator, options \\ [])

  @doc """
  Efficient implementation of inversing slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec inverse(t(), keyword()) :: {:ok, t()} | {:error, module()}
  def inverse(slots, options \\ [])
end
