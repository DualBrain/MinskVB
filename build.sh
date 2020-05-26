#!/bin/bash

# Vars
slndir="$(dirname "${BASH_SOURCE[0]}")/Minsk"

# Restore + Build
dotnet build "$slndir/minsk.sln" --nologo || exit

# Test
dotnet test "$slndir/Minsk.Tests" --nologo --no-build