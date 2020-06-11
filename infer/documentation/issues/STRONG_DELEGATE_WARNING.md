This check warns you when you have a property called delegate or variations
thereof which is declared strong. The idea is that delegates should generally be
weak, otherwise this may cause retain cycles.
