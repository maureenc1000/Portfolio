# Weather API: Current Conditions with OpenWeather

Calls the OpenWeather REST API from Python, handles errors, and turns the JSON response into a readable weather report with sunrise and sunset in local time.

## How the API key is kept secure
- The key is read from the `OPENWEATHER_API_KEY` environment variable, so it never appears in the notebook, its output, or this repository.
- The notebook never prints the key or the request URL (which would contain it).
- Setup: get a free key at openweathermap.org, run `setx OPENWEATHER_API_KEY "your-key-here"` once in PowerShell, then restart Jupyter.

## What it shows
- A place lookup (OpenWeather Geocoding API) and a weather request, with error handling for a rejected key (401), a city that is not found, other API errors and network failures
- A report of conditions, temperature, humidity, cloud cover, wind, pressure, sunrise and sunset
- Four examples: Dallas, Texas; London, Texas (shows the state is used); London, England; and a made-up city name to show the error handling

## Files
| File | What it is |
|---|---|
| `Weather_API.ipynb` | The notebook |

## Tools
Python (requests, datetime), OpenWeather Current Weather and Geocoding APIs, JSON

## Context
Master's coursework (DSC540), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Removed the hard-coded API key.** The key now comes from an environment variable. The old key was regenerated, since it had been published.
- **Fixed the location lookup.** The original sent the state as a separate `state_code` parameter, which the API ignores, so "London, TX" actually returned London, England. Sending `city,state,country` to the weather endpoint does not fix it (tested November 2026: it still returns London, England), so the notebook now looks up the coordinates with the Geocoding API, which does use the state, and requests the weather for those coordinates.
- Replaced the typed `input()` prompt with example calls so the notebook runs from start to finish, and added error handling for rejected keys and network problems
- Results change with the weather, so the output reflects the moment it was run
