execute sp_execute_external_script
	    @language = N'R',
	    @script = N'1 + 1',
	    @input_data_1 = N''
WITH RESULTS SETS (([col] int NOT NULL));