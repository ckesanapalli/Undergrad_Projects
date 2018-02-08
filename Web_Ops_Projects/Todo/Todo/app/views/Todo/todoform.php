<html>
<head>
<link rel="stylesheet" href="stylesheet.css" type="text/css" />
</head>

<body>
<form action="todoform.php"  method = "POST" >
	Username: <br/><input type="text" name="name"/><br/><br>
	Subject: <br/><input type="text" name="subject"/><br/><br>
	Description:<br/>
		<textarea wrap="virtual" name="data" rows=3	cols=20 maxlentgh=100></textarea><br>
	Deadline: <input type="date" name="deadline"/><br/><br>
	Priority: 
	<select name="prior">
		<option value="High">High</option>
		<option value="Medium">Medium</option>
		<option value="Low">Low</option>
	</select><br/><br>
	<input type="submit" value="Submit" name="enter"/>
	<button id='back'><a href="login.php">Back</a></button>
	
</form>

</body>
</html>



<?php
require('config.php');
if(isset($_POST['enter'])){
			$name= mysql_real_escape_string($_POST['name']);
			$subject= mysql_real_escape_string($_POST['subject']);
			$data= mysql_real_escape_string($_POST['data']);
			$deadline= mysql_real_escape_string($_POST['deadline']);
			$prior= mysql_real_escape_string($_POST['prior']);
			
			$sql=mysql_query("SELECT * FROM `todos` WHERE `data` = '$data'");
			if(mysql_num_rows($sql)>0){
				echo"<html>
					<body>
					<p class='error'>Sorry, that todo already exists.</p>
					
					</body>
					</html>
				";
			exit();
			}
			else{
				$sql1 = "INSERT INTO `todos`( `id`, `name`, `subject`, `data`, `deadline`, `prior`) VALUES ( NOT NULL, '$name', '$subject', '$data', '$deadline', '$prior')";
			mysql_query($sql1) or die(mysql_error());
			
		echo "<p class='success'>Your todo is Successfully added.<style>";
	}
}
?>
		
