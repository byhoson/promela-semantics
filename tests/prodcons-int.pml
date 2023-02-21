int turn = 0 

active proctype producer()
{
	do
	:: (turn == 0) ->
		printf("Produce\n");
		turn = 1
	od
}

active proctype consumer()
{
	do
	:: (turn == 1) ->
		printf("Consume\n");
		turn = 0
	od
}
