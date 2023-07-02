'''This is needed for testing on GitHub Actions due to `pip`
build-isolation shenanigans. If we're not careful, Hyrule's bytecode
will be generated with the latest release of Hy instead of the version
of Hy we actually requested.'''

import ast, subprocess
from pathlib import Path


p = Path('setup.py')

# Execute the first statement of `setup.py` to populate
# `dependencies`.
exec(compile(
    ast.Module(type_ignores = [], body =
        [ast.parse(p.read_text()).body[0]]),
    '', 'exec'))
# Install the dependencies with `--no-build-isolation` one at a time.
for d in dependencies:
    subprocess.run(check = True, args =
        ['pip', 'install', d, '--no-build-isolation'])
