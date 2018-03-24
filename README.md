# MinecraftCLI

MinecraftCLI is a command line application to join Minecraft servers with a valid Mojang or Legacy account, without needing the full Minecraft client. The application has several functions as described below.

## Usage

Run the binary and a % prompt will be displayed. Enter any of the following commands and use quit to exit.

### Help

Display all commands:

```javascript
help
```

### Profiles

Show available commands:

```javascript
profiles help
```

List all profiles:

```javascript
profiles list
```
Add a new profile:

```javascript
profiles new <login or Mojang account/email> 
```
Delete a profile:

```javascript
profiles delete <profile name>
```

### Servers

Show available commands:

```javascript
servers help
```

List all servers:

```javascript
servers list
```
Add a new server:

```javascript
servers new <name> <address: IP or DNS resolvable URL>
```
Delete a server:

```javascript
servers delete <server name>
```

### Connect

Connect to a server specifying a user profile and a server. The profile name must match one of those listed with `profiles list` and server name must match the server name that `servers list` displays.

Connect to a server:

```javascript
connect <profile> <server>
```

### Dimension

Display the Minecraft dimension that a player account is in. e.g. Overworld

```javascript
dimension
```
