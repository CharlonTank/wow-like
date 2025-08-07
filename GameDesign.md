# WoW-like Prototype - Game Design

## Core Loop

- Explore a shared open plane, see other players in real-time.
- Move, turn, sprint, jump; swap between third- and first-person camera.
- Select other players to target; cycle targets via Tab or click.

## Controls

- Movement: W/A/D/S (A/D strafe left/right)
- Turn: Arrow Left/Right
- Sprint: Shift (hold)
- Jump: Space (soft, floaty arc)
- Camera Toggle: C (Third-person ↔ First-person)
- Zoom: Q/E (in/out)
- Target Cycle: Tab or Click (cycles)
- Respawn: Button at bottom center

## Feel & Tuning

- Movement ramps up with acceleration; decelerates when inputs released.
- Jump uses gravity -9.8 m/s² with initial velocity 6.0 m/s.
- World bounds clamp to keep players within 500×500 area.
- Visual style inspired by Miyazaki: warm fog, soft palette, gentle tints.

## Networking

- Clients send `PlayerUpdate` snapshots: position (x,y,z) + yaw.
- Backend broadcasts `UpdatePlayers` with snapshot list to others.
- On connect, new client receives existing players; others see new player.

## Targeting

- `selectedPlayerId : Maybe ClientId` stored locally.
- Tab cycles through players by id order.
- Click selects the on-screen nearest player (screen-space projection).
- Selected target highlighted via subtle pulse and a ground ring under them.

## Next Steps

- Improve picking with raycasting and proper depth testing.
- Nameplates over players; simple emotes.
- Further smooth interpolation and yaw slerp.
- Basic obstacles/landmarks to navigate.
