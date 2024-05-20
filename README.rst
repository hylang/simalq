Infinitesimal Quest 2 + ε
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
`Source code (GitHub) <https://github.com/hylang/simalq>`_
------------------------------------------------------------

.. contents:: Contents:
   :backlinks: none

Introduction
============================================================

Infinitesimal Quest 2 + ε (or "SQ" for short; Python package name ``simalq``) is a Gauntlet-like turn-based puzzling dungeon crawler written to serve as an official example program for `Hy <http://hylang.org>`_. It's a reimplementation / remake / demake of `Yves Meynard <http://yvesmeynard.com>`_'s 1996 Macintosh game Infinity Quest II, abbreviated "IQ".

.. figure:: https://hylang.org/simalq/img/screenshot-New_First_Quest-l10-0cf40784f039b08828dc9e2a41bd9544ee646755.png
  :height: 300
  :alt: A terminal window with a colorful roguelike-esque map.

  The game running in Konsole on Lubuntu. Here our hero is getting overwhelmed by devils and bats on level 10 of New First Quest.

Features of SQ include:

- Playability on any platform with Python 3 and a compatible terminal emulator
- Glorious roguelike-like console display, with no distracting graphics, sounds, or mouse support
- `Oodles of monsters, magic items, and tricky terrain <http://hylang.org/simalq/doc/tilepedia>`__
- Deterministic, full-information gameplay
- Undo with infinite history
- Compatibility with quest (level) files in IQ's binary format
- An extensive test suite

Usage
============================================================

You can install SQ via `pip <https://packaging.python.org/en/latest/tutorials/installing-packages/>`__ from the `Python Package Index (PyPI) <https://pypi.org/project/simalq/>`__ with the command ``pip install simalq``, or from source with  ``pip install .``. For improved speed, try PyPy instead of CPython.

For the best display, you should use a terminal emulator that supports 24-bit color (I'm a fan of `Konsole <https://konsole.kde.org>`__), but SQ should be able to cope with less color support and use the best approximations available via its dependency `blessed <https://pypi.org/project/blessed/>`__. For Windows, SheepsTooth (an SQ fan) has kindly put together `a little step-by-step guide <https://github.com/hylang/simalq/files/14231807/Simalq.instructions.odt>`__ on getting the game to look nice in PowerShell.

Run SQ with ``python3 -m simalq``, or run the tests with ``pytest``. Use ``python3 -m simalq --help`` for options. A good way to start is Tutorial Quest: ``python3 -m simalq Tutorial_Quest``.

The first time that SQ requires IQ's original quests, it will download them automatically. The download is cached, so no Internet connection is needed afterwards.

See the `tilepedia <http://hylang.org/simalq/doc/tilepedia>`__ for an HTML compendium of tile info pages.

The story so far
============================================================

Her Royal Highness Princess Triskaidecagonn XIII (whose friends call her "Tris"), tiring of her studies, shut the heavy grimoire of nonstandard analysis. "For too long I have been obliged to concern myself with tedious scholarship" she lamented aloud "while my older brother Argonn has sallied forth on many an heroic quest. Here I sit, idling, with nary an opportunity to test my own skills in the sword, the bow, and the sliding-block puzzle."

"Aha!" cried the fell wizard Idok, deep in his underground laboratory, who had been spying on the princess through his mystical Macintosh LC so he could copy her homework answers. "Here is an opportunity to teach you, young princess, to be careful what you wish for." He typed a dread incantation in Hy, the long-dead language of squids born of snakes, and in a swarm of foul parentheses, Tris was carried off to a vast dungeon deep beneath the faraway elven land of Québec. The place looked familiar, and Tris realized that Idok had plagiarized pretty much the whole thing from the thesis of his doctoral advisor, the wicked sorcerer Karvarel. Her brother had braved these very dungeons years ago. She didn't have his memoirs handy, but she did know one really good magic spell, which allowed her to predict the future. Now, with this clairvoyance, her trusty sword and bow, and anything handy she happens to find lying around, Tris must escape the dungeon or die in the attempt. And if she can stuff her pockets with loot on the way, that would really help with her kingdom's latest financial crisis.

Differences from IQ
============================================================

Apart from cosmetic and other interface differences, the chief way SQ differs from IQ in its design is its commitment to determinism. SQ replaces IQ's random mechanics, such as monster pathfinding, with deterministic equivalents, sometimes making aspects of the game stateful that were previously stateless. SQ also deliberately omits interface hijinks like darkness and confusion. SQ adds flavor text for tiles, mid-game saving and loading, undo, visibility of the map outside the hero's shooting radius, fixes to bugs and weird behavior (e.g., winning with a "you have died" message if you win the game and die to poison on the same turn), removal of many engine limits (e.g., max and min level size), and the ability to adjust some core game rules (e.g., whether monsters can walk on items).

The original IQ map layouts have a number of swastika-like designs. Yves Meynard, the author of IQ, explained to me in 2023 "I meant them to be a shorthand mark of evil. Looking back at IQ2, it's the one thing I wish I hadn't done, because there was a potential for misreading the symbol." I agree that without an explanatory context, like fighting Nazis, swastikas are uncomfortable to see in a video game. So, when SQ loads the original levels, it censors the swastikas by replacing them with new layouts I've made up for the corresponding level regions. I leave skull designs untouched, although the Totenkopf was also a Nazi symbol, because a generic skull shape is more universal and less Nazi-associated in contemporary times.

While Yves kindly provided me with IQ's source code for reference, SQ is an original work that doesn't substantively copy IQ at the code level, and has many fine differences (deliberate, and probably also accidental) in behavior.

Implementation
============================================================

SQ is reasonably polished and allows playing through several complete quests. Not quite all of IQ's tile types are implemented, and so a few of its original quests (Nightmare 2, Delirium, and the original tutorial Boot Camp 2) aren't completely playable. You can try them if you want, but they aren't listed by ``--quests``.

Overall, the code is in good shape and provides examples of solving many different kinds of problems in Hy. Some files use `Outli <https://github.com/jdtsmith/outli>`__ headers in comments for organization. Among the things you can see demonstrated in SQ are:

- Using macros to streamline the definition of many similar classes: see ``defdataclass`` and ``deftile`` (try looking at uses of them first before getting into the hairy details of their implementation)
- Managing global variables and several notions of global state: see ``Global``
- Writing a terminal-based game with `blessed <https://pypi.org/project/blessed>`__
- Parsing a novel binary format with `construct <https://pypi.org/project/construct>`__
- Generating HTML with a minimal ``ElementTree``-like interface: see ``simalq.tile.tilepedia``
- Testing a Hy program with `pytest <https://pytest.org>`__ (and with tests that are themselves written in Hy)
- Creating a new quest for SQ: duplicate the file ``tutorial.hy``, edit it to taste, and put it in the same directory (``quest_definition``)

Some of SQ's approach and a little of its code descends from my earlier unfinished game `Rogue TV <https://github.com/Kodiologist/Rogue-TV>`__.

Version history
============================================================

This section lists the most important user-visible changes in each release. Typically, saved games for each version *aren't* compatible with other versions, but release numbers marked with an asterisk (*) should be compatible with saved games from the previous release. Unless making a backward-compatible upgrade of this kind, you should delete all your saved games before upgrading the game.

- 1.0.1* (2024-05-20): Largely internal changes.
- 1.0.0 (2024-03-01): Various improvements to the display.
- 0.6.0 (2024-02-19): The IQ quest BoneQuest is now fully playable.
- 0.5.0 (2024-01-10): Largely internal changes. The new script ``util/gate_map.hy`` may be useful for mapping mazes of teleportation gates, such as New Nightmare level 10.
- 0.4.0* (2023-09-08): SQ now uses `platformdirs <https://pypi.org/project/platformdirs>`__ to set data directories.
- 0.3.0 (2023-09-01): There are now difficulty options available through the command line. Variability in the movement of wandering monsters, such as bats, has been improved.
- 0.2.0* (2023-07-23): The IQ quest New Nightmare has been denazified and added to ``--quests``.
- 0.1.0 (2023-07-13): First playable release.

License
============================================================

This program is copyright 2023, 2024 Kodi B. Arfer.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`GNU General Public License`: http://www.gnu.org/licenses/
