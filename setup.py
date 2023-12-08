dependencies = [
    'hy @ git+https://github.com/hylang/hy@408cc7560bb89516208966104641fe2394e6aabe',
    'hyrule @ git+https://github.com/hylang/hyrule@2aabac6506053cc49d38552973924280c3349886',
    'toolz >= 0.12.0',
    'construct >= 2.10.68',
    'blessed >= 1.20.0',
    'platformdirs >= 4.0.0']

import setuptools
from pathlib import Path

setuptools.setup(
    name = 'simalq',
    version = '0.0.0',
    author = 'Kodi B. Arfer',
    description = 'Infinitesimal Quest 2 + ε: A turn-based puzzling dungeon crawler',
    long_description = Path('README.rst').read_text(),
    long_description_content_type = 'text/x-rst',
    project_urls = {
        'Source Code': 'https://github.com/hylang/simalq'},
    python_requires = '>= 3.10',
    install_requires = dependencies,
    packages = setuptools.find_packages(),
    package_data = dict(simalq = [
        str(p.relative_to('simalq'))
        for p in Path('simalq').rglob('*.hy')]),
    classifiers = [
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3'])
