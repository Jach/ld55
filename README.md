This was supposed to be a simple game for [Ludum Dare 55](https://ldjam.com/events/ludum-dare/55)
(theme: summoning) but I lost motivation and interest.
I worked on it for maybe 5ish hours on the 13th, then another 2ish hours on the 20th.

Code quality is pretty bad, I thought about not letting this see the light of day, but it at least serves as a (poor) example of using [lgame](https://github.com/Jach/lgame) so...
No one will probably look at it anyway since I don't intend to advertise it.

Also art is very ugly, final state of "gameplay" isn't fun, but whatever, I at least had some fun
distracting myself from life for a bit.

Source code is in the public domain / unlicensed, do what you want. Don't
suggest using this method for infinite scrolling background, instead create an actual
camera system and separate world coordinates from render space coordinates.

(Final design note / why is ironclad included but not used: I wanted to add
rare "letters" that can drop from killed potatoes, with a mysterious sha256 hash
always displayed on screen. When you collect 4 letters, the next summon is no
longer a potato, and you basically win. For some reason I thought it'd be fun to
guarantee the displayed hash has the "letter" prefixes after you collect each
one, and the "reward" of winning is a string that hashes to c040xxxx... (Since
4, written in an open handwriting style (see included but not used font), sort
of looks like a Y.) A harder version could continue until getting 7 (T) and e.)
