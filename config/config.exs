import Config

config :logger,
  level: :info,
  backends: [:console],
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

config :telemetria,
  otp_app: :tempus,
  enabled: false,
  applications: [],
  events: [],
  polling: [enabled: false]

if File.exists?("config/#{Mix.env()}.exs"), do: import_config("#{Mix.env()}.exs")
