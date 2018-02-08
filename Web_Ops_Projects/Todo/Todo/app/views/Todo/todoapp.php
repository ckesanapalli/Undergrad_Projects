<?php
require('config.php');
if(isset($_POST['enter'])){
	
	$email= mysql_real_escape_string($_POST['email']);
	$pass= mysql_real_escape_string($_POST['pass']);

	$sql=mysql_query("SELECT * FROM `users` WHERE `email` = '$email'");	
	while($row = mysql_fetch_assoc($sql)){
		$dbuser=$row['name'];
	}
	if(mysql_num_rows($sql)>0){
			
			
	
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
		<script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
		<script src="backbone.js" type="text/javascript"></script>
		<script src="underscore.js" type="text/javascript"></script>
		<title>To-Do</title>
		<link rel="stylesheet" href="stylesheet.css" type="text/css" />
	</head>
	
	<body>
		<center><h1><?php echo $dbuser;?>'s Todos </h1></center>
		<marquee>Write your name '<?php echo $dbuser;?>' in New Todo form</marquee>
		<button id='logout'><a href="index.php">Log out</a></button><br><br>
		
		<table class='todos'>
			<tr>
				<td></td>
				<td><b>Subject</b></td>
				<td><b>Description</b></td>
				<td><b>Priority</b></td>
				<td><b>Deadline</b></td>
				
			</tr><hr>
			<form action="mysql.php", method='POST'>
		
		<?php
		
			$res = mysql_query("SELECT id FROM todos");
			for ($i=1; $i <= mysql_num_rows($res); $i++) {
				$to= mysql_query("SELECT * FROM `todos` WHERE `id` = '$i' AND `name` = '$dbuser'");
				if(mysql_num_rows($to)>0){
					while($row = mysql_fetch_assoc($to)){
						$psubject=$row['subject'];
						$pdata=$row['data'];
						$pprior=$row['prior'];
						$pdeadline=$row['deadline'];
					}
					
					echo"
					<html>
					<body>
					<tr id= $i>
						<td><input type='checkbox' name='$i' value='todo' /></td>
						<td>$psubject</td>
						<td>$pdata</td>
						<td>$pprior</td>
						<td>$pdeadline</td>				
					</tr>
					</body>
					</html>
					";	
				}
			}?>
		</table><br><br>
		
		<input type='submit' name='delete' value='Delete' /><br>
		<input type='submit' name='completed' value='Completed' />
		</form>
		
		<br><br><center>
		<button id='newtodo'><a href="todoform.php">New Todo</a></button></center>
	</body>
</html>
<?php
	}
	else{
		echo "Wrong U/P combination";
	}
}
?>
