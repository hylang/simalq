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
    install_requires = [
        'hy @ git+https://github.com/hylang/hy@0e3409abf38bc7271a28b4388f146f24c93efeb1',
        'hyrule @ git+https://github.com/Kodiologist/hyrule@4978273ee5ee0c1d52012a87f6fbbc6abf3979e9',
        'toolz >= 0.12.0',
        'construct >= 2.10.68',
        'blessed >= 1.20.0'],
    packages = setuptools.find_packages(),
    package_data = dict(simalq = [
        str(p.relative_to('simalq'))
        for p in Path('simalq').rglob('*.hy')]),
    classifiers = [
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3'])
