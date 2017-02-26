<?php
namespace app\helpers;
/**
 * Класс для работы с облаком https://cloud.mail.ru.
 * @author <popov_si@mail.ru>, <antikillerxxl@gmail.com>
 * @license GNU GPL v2.0
 */

class CloudMailRu {
	public
		$user,
		$pass,
		$domain,
		$dir,
		$token,
		$x_page_id,
		$build,
		$upload_url,
		$ch,
		$cookie;

	function __construct($user, $pass, $domain='mail.ru') {
		$this->user = $user;
		$this->pass = $pass;
		$this->domain = $domain;
		$this->dir = dirname(__FILE__);
		$this->token = '';
		$this->x_page_id = '';
		$this->build = '';
		$this->upload_url = '';
		$this->ch;
		$this->cookie = __DIR__. "/cookies.txt";
		//touch($this->cookie);
	}
	
	function __destruct() {
		//if (file_exists($this->cookie))unlink($this->cookie);
	}	

	function login() {

		$url = 'http://auth.mail.ru/cgi-bin/auth';

		$post_data = [
			"page" => "https://cloud.mail.ru/",
			"FailPage" => "",
			"Domain" => $this->domain,
			"Login" => $this->user,
			"Password" => $this->pass,
			"new_auth_form" => "1"
		];

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		Utils::fileLog($result['result']);
		if ($result['success']) {
			if ($this->getToken()) {
				//$this->secstep();
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

	function firststep(){
		$url = 'http://auth.mail.ru/cgi-bin/auth';
		$url = 'http://lightcab/web/index.php?r=test';

		$post_data = [
			//"page" => "https://cloud.mail.ru/",
			//"FailPage" => "",
			"Domain" => $this->domain,
			"Login" => $this->user,
			"Password" => $this->pass,
			//"new_auth_form" => "1"
		];

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		Utils::fileLog($result['result']);
		if ($result['success']) {
			return self::getTokenFromText($result['result']);
		} else {
			return false;
		}
	}

	function secstep($csrf,$authcode){
		$url = 'https://auth.mail.ru/cgi-bin/secstep';

		$post_data = [
			'csrf' => $csrf,
			'Login' => $this->user.'@'.$this->domain,
			'AuthCode' => $authcode,
//			'Resend' => 1
		];
Utils::log($post_data);
		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		Utils::fileLog($result);
		if ($result['success']) {
			if ($this->getToken()) {
				Utils::log($this->token,'token');
				Utils::log($this->x_page_id,'x_page_id');
				Utils::log($this->build,'$this->build');
				Utils::log($this->upload_url ,'$this->upload_url ');
				return true;
			} else {
				Utils::log('error');
				return false;
			}
		} else {
			return false;
		}
	}

	private function getToken() {

		$url = 'https://cloud.mail.ru/?from=promo&from=authpopup';

		$this->_curl_init($url);
		$result = $this->_curl_exec();
		if ($result['success']) {
			$token = self::getTokenFromText($result['result']);
			if ($token == '') {
				return false;
			} else {
				$this->token = $token;
				$this->x_page_id = self::get_x_page_id_FromText($result['result']);
				$this->build = self::get_build_FromText($result['result']);
				$this->upload_url = self::get_upload_url_FromText($result['result']);
				return true;
			}
		} else {
			return false;
		}
	}

	function getDir($dir) {
		$dir = str_replace('/', '%2F', $dir);
		$url = 'https://cloud.mail.ru/api/v2/folder?home=%2F'
			.'&sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}'
			.'&offset=0'
			.'&limit=10000'
			.'&home='.$dir
			.'&api=2'
			.'&build='.$this->build
			.'&x-page-id='.$this->x_page_id
			.'&email='.$this->user.'%40'.$this->domain
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&token='.$this->token.'&_=1433249148810';
		$this->_curl_init($url);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return json_decode($result['result'],true);
		} else {
			return "error";
		}
	}

	private function getShard(){
		$url = 'https://cloud.mail.ru/api/v2/dispatcher/';

		$post_data = ''
			.'api=2'
			.'&build='.$this->build
			.'&email='.$this->user.'%40'.$this->domain
			.'&token='.$this->token
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&x-page-id='.$this->x_page_id;
		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			$response = json_decode($result['result'],true);
			return $response['body']['get']['0']['url'];
		} else {
			return "error";
		}
	}

	function addDir($dir) {
		$url = 'https://cloud.mail.ru/api/v2/folder/add';
		$dir = str_replace('/', '%2F', $dir);
		$post_data = ''
			.'api=2'
			.'&build='.$this->build
			.'&conflict=rename'
			.'&email='.$this->user.'%40'.$this->domain
			.'&home='.$dir
			.'&token='.$this->token
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&x-page-id='.$this->x_page_id;

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return json_decode($result['result']);
		} else {
			return "error";
		}
	}

	function loadFileAhdPublish($file_name, $dir_cloud) {

		$result_load = $this->loadFile($file_name, $dir_cloud);
		if ($result_load !== "error") {

			$result_publish = $this->publishFile($result_load->body);
			if ($result_publish !== "error") {
				if ($result_publish->status == '200') {
					return 'https://cloud.mail.ru/public/'.$result_publish->body;
				} else {
					return "error";
				}
			} else {
				return "error";
			}
		} else {
			return "error";
		}
	}

	function loadFile($file_name, $dir_cloud) {

		$arr = $this->loadFile_to_cloud($file_name);
		if ($arr !== "error") {

			$result_ = $this->addFile_to_cloud($arr, $dir_cloud);
			if ($result_ !== "error") {
				return json_decode($result_);
			} else {
				return "error";
			}
		} else {
			return "error";
		}
	}

	private function loadFile_to_cloud($file_name) {

		$_time = time().'0246';

		$url = $this->upload_url
			.'?cloud_domain=2'
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&fileapi'.$_time;

		$post_data = ["file" => curl_file_create($file_name)];

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			$arr = explode(';', $result['result']);
			if (strlen($arr[0]) == 40) {
				$arr[1] = intval($arr[1]);
				return $arr;
			} else {
				return "error";
			}
		} else {
			return "error";
		}
	}

	private function addFile_to_cloud($arr, $dir_cloud) {

		$url = 'https://cloud.mail.ru/api/v2/file/add';

		$post_data = ''
			.'api=2'
			.'&build='.$this->build
			.'&conflict=rename'
			.'&email='.$this->user.'%40'.$this->domain
			.'&home='.$dir_cloud
			.'&hash='.$arr[0]
			.'&size='.$arr[1]
			.'&token='.$this->token
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&x-page-id='.$this->x_page_id;

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return $result['result'];
		} else {
			return false;
		}
	}

	function removeFile_from_cloud($dir_cloud) {

		$url = 'https://cloud.mail.ru/api/v2/file/remove';

		$post_data = ''
			.'api=2'
			.'&build='.$this->build
			.'&email='.$this->user.'%40'.$this->domain
			.'&home='.$dir_cloud
			.'&token='.$this->token
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&x-page-id='.$this->x_page_id;

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return $result['result'];
		} else {
			return false;
		}
	}

	function renameFile_on_cloud($oldName,$newName) {

		$url = 'https://cloud.mail.ru/api/v2/file/rename';

		$post_data = ''
			.'api=2'
			.'&build='.$this->build
			.'&email='.$this->user.'%40'.$this->domain
			.'&home='.$oldName
			.'&name='.$newName
			.'&token='.$this->token
			.'&x-email='.$this->user.'%40'.$this->domain
			.'&x-page-id='.$this->x_page_id;

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return $result['result'];
		} else {
			return false;
		}
	}

	function publishFile($file_path) {

		$url = 'https://cloud.mail.ru/api/v2/file/publish';

		$post_data = ''
				. 'api=2'
				. '&build='.$this->build
				. '&email='.$this->user.'%40'.$this->domain
				. '&home='.$file_path
				. '&token='.$this->token
				. '&x-email='.$this->user.'%40'.$this->domain
				. '&x-page-id='.$this->x_page_id;

		$this->_curl_init($url);
		$this->_curl_post($post_data);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return json_decode($result['result']);
		} else {
			return false;
		}
	}

	public function getFile($file){
		$shard = $this->getShard();
		$file_url = $shard.$file;
		$this->_curl_init($file_url);
		$result = $this->_curl_exec();
		if ($result['success']) {
			return $result['result'];
		} else {
			return false;
		}
	}

	//------------------------------

	private function _curl_init($url) {
		$this->ch = curl_init();
		curl_setopt($this->ch, CURLOPT_URL, $url);
//		curl_setopt($this->ch, CURLOPT_REFERER, $url);
		curl_setopt($this->ch, CURLOPT_USERAGENT, 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17');
		curl_setopt($this->ch, CURLOPT_FOLLOWLOCATION, true);
		curl_setopt($this->ch, CURLOPT_RETURNTRANSFER, true);
		curl_setopt($this->ch, CURLOPT_SSL_VERIFYPEER, false);
		curl_setopt($this->ch, CURLOPT_COOKIEFILE,$this->cookie);
		curl_setopt($this->ch, CURLOPT_COOKIEJAR, $this->cookie);
	}

	private function _curl_post($post_data) {
//		curl_setopt($this->ch, CURLOPT_HTTPHEADER,array('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17'));
		curl_setopt($this->ch, CURLOPT_POST, true);
		curl_setopt($this->ch, CURLOPT_POSTFIELDS, $post_data);
	}

	private function _curl_exec() {
		$result_ = curl_exec($this->ch);
		$status = curl_errno($this->ch);
		Utils::log($status,'status');
//		Utils::log($result_,'$result_');
		curl_close($this->ch);
		if ($status == 0 && !empty($result_)) {
			return [
				"success"=> true,
				"result" => $result_
			];
		} else {
			return [
				"success"=> false,
				"result" => $status
			];
		}
	}

	//-----------------------------

	private static function getTokenFromText($str) {
		$start = strpos($str, '"csrf"');
		if ($start > 0) {
			$start = $start + 8;
			$str_out = substr($str, $start, 32);
			return $str_out;
		} else {
			return '';
		}
	}

	private static function get_x_page_id_FromText($str) {
		$start = strpos($str, '"x-page-id": "');
		if ($start > 0) {
			$start = $start + 14;
			$str_out = substr($str, $start, 11);
			return $str_out;
		} else {
			return '';
		}
	}

	private static function get_build_FromText($str) {
		$start = strpos($str, '"BUILD": "');
		if ($start > 0) {
			$start = $start + 10;

			$str_temp = substr($str, $start, 100);

			$end = strpos($str, '"');

			$str_out = substr($str_temp, 0, $end - 1);
			return $str_out;
		} else {
			return '';
		}
	}

	private static function get_upload_url_FromText($str) {
		$start = strpos($str, 'mail.ru/upload/"');
		if ($start > 0) {
			$start1 = $start - 50;
			$end1 = $start + 15;
			$length = $end1 - $start1;
			$str_temp = substr($str, $start1, $length);

			$start2 = strpos($str_temp, 'https://');
			$str_out = substr($str_temp, $start2, strlen($str_temp) - $start2);
			return $str_out;
		} else {
			return '';
		}
	}

}
