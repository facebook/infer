class Test {

	public void select() {
		String a = "SELeCT id FROM ";
		String b = "table";
		String c = " WHERE something";
		String d = a + b + c;
	}

	public void insert() {
		int id = 10;
		String a = "INSERT into table VALUES (1) WHERE id=";
		String q = a + Integer.toString(id);
	}

}
