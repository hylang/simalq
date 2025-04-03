import setuptools
from pathlib import Path

setuptools.setup(
    name = 'simalq',
    version = '2.0.0',
    author = 'Kodi B. Arfer',
    description = 'Infinitesimal Quest 2 + ε: A turn-based puzzling dungeon crawler',
    long_description = Path('README.rst').read_text(),
    long_description_content_type = 'text/x-rst',
    project_urls = {
        'Homepage': 'http://hylang.org/simalq',
        'Source Code': 'https://github.com/hylang/simalq'},
    python_requires = '>= 3.10',
    install_requires = [
        'hy >= 1',
        'hyrule >= 1',
        'toolz >= 1',
        'construct >= 2.10.70',
        'blessed >= 1.20.0',
        'platformdirs >= 4.3.6',
        'metadict >= 0.1.3'],
    packages = setuptools.find_packages(),
    package_data = dict(simalq = [
        str(p.relative_to('simalq'))
        for p in Path('simalq').rglob('*.hy')]),
    classifiers = [
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Operating System :: OS Independent',
        'Programming Language :: Hy',
        'Topic :: Games/Entertainment :: Puzzle Games'])
