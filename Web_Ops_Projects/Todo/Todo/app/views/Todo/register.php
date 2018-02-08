<link rel="stylesheet" href="stylesheet.css" type="text/css" />
<form action="register.php" method="POST">
	Name:<br><input type="text" name="name"/><br/>
	Email: <br><input type="text" name="email1"/><br/>
	Confirm Email: <br><input type="text" name="email2"/><br/>
	Password:<br> <input type="password" name="pass1"/><br/>
	Confirm Password:<br> <input type="password" name="pass2"/><br/><br>
	<input type="submit" value="Register" name="submit"/>
		
</form>
<?php
require('config.php');
if(isset($_POST['submit'])){
	$email1=$_POST['email1'];
	$email2=$_POST['email2'];
	$pass1=$_POST['pass1'];
	$pass2=$_POST['pass2'];
	if($email1 == $email2){
		$e1='@';
		$e2='.';
		$c=0;
		for($i=1;$i< strlen($email1);$i++){
			if(($email1[$i] =='@' || $email1[$i] =='.') & strlen($email1)>=5)
			{
				$c++;
			}
		}
		if($c<2){
			echo " Your Email is Invalid";
			echo 
			exit();
		}
			
		if($pass1 ==$pass2){
			$name= mysql_real_escape_string($_POST['name']);
			$email1= mysql_real_escape_string($email1);
			$email2= mysql_real_escape_string($email2);
			$pass1= mysql_real_escape_string($pass1);
			$pass2= mysql_real_escape_string($pass2);
			
			$sql=mysql_query("SELECT * FROM `users` WHERE `email` = '$email1'");
			if(mysql_num_rows($sql)>0){
			echo"Sorry, that user already exists.";
			exit();
			}
			else{
				echo "<html><b>Your are Successfully registered.</b> <br><br><button><a href='login.php'>Now Login</a></button><br><br></html>";
			}
			$sql1 = "INSERT INTO `users`( `id`, `name`, `email`, `password`) VALUES ( NULL, '$name', '$email1', '$pass1')";
			mysql_query($sql1) or die(mysql_error());
		}
		else{
			echo"Sorry, your passwords do not match";
			exit();
		}
	}
	else{
		echo"Sorry, your emails do not match";
		exit();
	}
}

?>



