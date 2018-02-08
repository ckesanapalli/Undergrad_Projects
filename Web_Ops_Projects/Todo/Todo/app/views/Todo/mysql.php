<?php
require 'config.php';

$sql1 = "CREATE TABLE users (id INT(200), name VARCHAR(20), email VARCHAR(20), password VARCHAR(20), date DATE)";
$sql2 = "CREATE TABLE todos (id INT(200), name VARCHAR(20), subject VARCHAR(20), data TEXT, deadline VARCHAR(10),prior VARCHAR(10))";

mysql_query($sql1);
mysql_query($sql2);
echo "hi!";


if(isset($_POST['delete'])){	
	$res = mysql_query("SELECT id FROM todos");
	for ($i=1; $i <= mysql_num_rows($res); $i++) {
		if($_POST['$i']){
			$sql=mysql_query("DELETE * FROM `users` WHERE `id`='$i'");	
		}
	}
}
if(isset($_POST['completed'])){	
	$res = mysql_query("SELECT id FROM todos");
	for ($i=1; $i <= mysql_num_rows($res); $i++) {
		if($_POST['$i']){
			echo "
				<html>
				<style>	
					.'$i'{
						background-color: Green;
					}
				</style>
				</html>
			";
		}
	}
}
?>