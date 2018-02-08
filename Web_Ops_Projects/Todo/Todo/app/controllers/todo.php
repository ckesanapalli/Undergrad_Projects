<?php
	class todo_controller extends base_controller(
		public $restful=true;
		public function get_index(){
			return View::make('todo.index');
			
		}
	}
?>