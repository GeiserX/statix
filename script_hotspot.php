<?php
use PEAR2\Net\RouterOS;
require_once '/home/tecnico/PEAR/Autoload.php';

header('Content-Type: text/plain');

print "1";

try {
    
    print "2";
    
    $util = new RouterOS\Util($client = new RouterOS\Client('x.x.x.x', 'admin', 'PASSWORD'));
    
    print "3";
    
    foreach ($util->setMenu('/log')->getAll() as $entry) {
        echo $entry('time') . ' ' . $entry('topics') . ' ' . $entry('message') . "\n";
    }
} catch (Exception $e) {
    echo 'Unable to connect to RouterOS.';
}

?>

