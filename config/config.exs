import Config

config :logger,
  level: :info,
  backends: [:console],
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

config :tempus, telemetria?: true

config :telemetria,
  otp_app: :tempus,
  enabled: true,
  polling: [enabled: false],
  applications: [],
  events: [
    [:tempus, :add],
    [:tempus, :next_busy],
    [:tempus, :next_free],
    [:tempus, :slots, :add],
    [:tempus, :slots, :inverse],
    [:tempus, :slots, :merge]
  ]

if File.exists?("config/#{Mix.env()}.exs"), do: import_config("#{Mix.env()}.exs")
