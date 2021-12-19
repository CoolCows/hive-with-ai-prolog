# hive-with-ai-prolog

Implemention in prolog of known [board game Hive](https://www.gen42.com/games/hive). We don't see ourselves in the future improving  the project or fixing bugs, but feel free to make all the improvements you want or contacting us for help.

## Using SWI-Prolog

```bash
cd gui
swipl main.pl
```

Currently File > New Game > Local game works out of the box.

## To Start Up AI

 Create a `db` folder inside `ai` (yes, it is ugly). Then you can play against a dumb IA by starting a New Game in the GUI and selecting `against Ai` or `AI vs AI`.

AI uses Monte Carlo Search Trees (MCSTs) and it is currently untrained so it will be dumb as f*ck.

To train the AI

```bash
cd gui
python run_train.py <times>
```

where `<times>` represents the amount of matches the IA is going to play against itself. All nodes are stored persistently  `ai/db/tree_db.pl`.

When the AI has found an average of 2000 ~ 4000 leaf nodes it start doing some good looking stuff, although not enough.

## Possible Improvements

* (very easy) Improve game art (which is already awesome btw)  by modifying/changing bug pictures in `gui/graphics/xpm`. Replace by files with the same name. Images must be 70x70 pxs. Be careful new bugs are not bigger than the game hexagon.
* (easy) Add logic to to detect end game states in `gui`. A solution can be to obtain current game status by a call to `gui_api` and if it is not `non_terminal` do not accept any more events.
* (easy) Correctly move `game\gui_api.pl` to `gui\gui_api.pl` and update code properly so no errors occurs. `gui_api.pl` is out of place right now.
* (easy) Add logic to automatically create a `db` folder in `ai` when this folder does not exists.
* (medium) Make possible restarting a game without needing to exit current process.
* (hard) Make MCST multithreading. It will need to use mutexes to avoid race conditions when writing to `ai/db/tree_db.pl`. It will also need to make global game status local to each thread, since simulations are run over this by changing it, and setting it back to the initial state when finished.
* (hard) Add an efficient way of storing the MCST nodes. Currently `persistent` library is used, when around 4000 branches have been explored the database weights around 80 mbs, and takes around 3 seconds to load.
* Any other thing you think is cool.

