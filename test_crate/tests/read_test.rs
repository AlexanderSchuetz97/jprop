use jni_simple::{JNIEnv, JNI_CreateJavaVM_with_string_args, JNI_VERSION_1_8};
use jprop::{parse_bytes_iso_8859_1_to_map, parse_bytes_utf8_to_map, parse_iso_8859_1, parse_iso_8859_1_to_map, parse_utf8, parse_utf8_to_map, Element, ParserPosition, PropertyHandler};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::ptr::null_mut;
use std::sync::Mutex;

static MUTEX: Mutex<()> = Mutex::new(());

fn get_jvm() -> JNIEnv {
    let _guard = MUTEX.lock();
    let jvm = unsafe {
        _ = jni_simple::load_jvm_from_java_home();
        assert!(jni_simple::is_jvm_loaded());
        jni_simple::JNI_GetCreatedJavaVMs_first()
            .expect("Failed to get jvm")
            .unwrap_or_else(|| {
                let (jvm, _) =
                    JNI_CreateJavaVM_with_string_args::<&str>(JNI_VERSION_1_8, &[], true)
                        .expect("Failed to create java VM");
                jvm
            })
    };

    unsafe {
        let Ok(env) = jvm.GetEnv::<JNIEnv>(JNI_VERSION_1_8) else {
            return jvm
                .AttachCurrentThread_str(JNI_VERSION_1_8, (), null_mut())
                .unwrap();
        };

        env
    }
}

fn jvm_prop_to_rust_map(path: &str, charset: &str) -> HashMap<String, String> {
    unsafe {
        let jvm = get_jvm();
        let system_class = jvm.FindClass("java/lang/System");
        assert!(!system_class.is_null());
        let out_field = jvm.GetStaticFieldID(system_class, "err", "Ljava/io/PrintStream;");
        assert!(!out_field.is_null());
        let out = jvm.GetStaticObjectField(system_class, out_field);
        assert!(!out.is_null());
        let print_stream = jvm.GetObjectClass(out);
        assert!(!print_stream.is_null());
        let println = jvm.GetMethodID(print_stream, "println", "(Ljava/lang/String;)V");
        assert!(!println.is_null());

        let prop_class = jvm.FindClass("java/util/Properties");
        assert!(!prop_class.is_null());
        let prop_load = jvm.GetMethodID(prop_class, "load", "(Ljava/io/Reader;)V");
        assert!(!prop_load.is_null());
        let prop_constr = jvm.GetMethodID(prop_class, "<init>", "()V");
        assert!(!prop_constr.is_null());
        let prop = jvm.NewObject0(prop_class, prop_constr);
        assert!(!prop.is_null());
        let prop_get = jvm.GetMethodID(
            prop_class,
            "getProperty",
            "(Ljava/lang/String;)Ljava/lang/String;",
        );
        assert!(!prop_get.is_null());
        let prop_keys = jvm.GetMethodID(prop_class, "stringPropertyNames", "()Ljava/util/Set;");
        assert!(!prop_keys.is_null());

        let set_class = jvm.FindClass("java/util/Set");
        assert!(!set_class.is_null());
        let set_to_array = jvm.GetMethodID(set_class, "toArray", "()[Ljava/lang/Object;");
        assert!(!set_to_array.is_null());

        let fais_class = jvm.FindClass("java/io/FileInputStream");
        assert!(!fais_class.is_null());
        let fais_constr = jvm.GetMethodID(fais_class, "<init>", "(Ljava/lang/String;)V");
        assert!(!fais_constr.is_null());
        let fais_close = jvm.GetMethodID(fais_class, "close", "()V");
        assert!(!fais_close.is_null());
        let path_jstring = jvm.NewStringUTF(path);
        assert!(!path_jstring.is_null());
        let fais = jvm.NewObject1(fais_class, fais_constr, path_jstring);
        assert!(!fais.is_null());

        let isr_class = jvm.FindClass("java/io/InputStreamReader");
        assert!(!isr_class.is_null());
        let isr_constr = jvm.GetMethodID(
            isr_class,
            "<init>",
            "(Ljava/io/InputStream;Ljava/lang/String;)V",
        );
        assert!(!isr_constr.is_null());

        let utf8_str = jvm.NewStringUTF(charset);

        let isr = jvm.NewObject2(isr_class, isr_constr, fais, utf8_str);
        assert!(!isr.is_null());

        jvm.CallObjectMethod1(prop, prop_load, isr);
        assert!(!jvm.ExceptionCheck());
        jvm.CallObjectMethod0(fais, fais_close);
        jvm.ExceptionClear();

        let prop_keys = jvm.CallObjectMethod0(prop, prop_keys);
        assert!(!prop_keys.is_null());
        let prop_key_array = jvm.CallObjectMethod0(prop_keys, set_to_array);
        assert!(!prop_key_array.is_null());

        let mut result = HashMap::new();
        let nkeys = jvm.GetArrayLength(prop_key_array);
        for i in 0..nkeys {
            let element_key = jvm.GetObjectArrayElement(prop_key_array, i);
            assert!(!element_key.is_null());
            let element_value = jvm.CallObjectMethod1(prop, prop_get, element_key);
            assert!(!element_value.is_null());
            //jvm.CallObjectMethod1(out, println, element_value);
            let key = jvm
                .GetStringChars_as_string(element_key)
                .expect("Failed to turn jstring into rust String");


            let value = jvm
                .GetStringChars_as_string(element_value)
                .expect("Failed to turn jstring into rust String");
            result.insert(key, value);
        }

        result
    }
}

#[derive(Default)]
struct Handler(HashMap<String, String>);

impl PropertyHandler for Handler {
    fn handle(&mut self, _position: &ParserPosition, value: Element) -> bool {
        if let Element::Value(key, value) = value {
            self.0.insert(key, value);
        }
        true
    }
}

pub fn do_test_utf8(path: &str) {
    let mut file = File::open(path).unwrap();
    let java_map = jvm_prop_to_rust_map(path, "UTF-8");
    let data = parse_utf8_to_map(&mut file).expect("Failed to parse file");

    for (key, val) in data.iter() {
        assert_eq!(java_map.get(key), Some(val), "{}={}", key, val);
    }
    assert_eq!(java_map.len(), data.len());

    let mut file = File::open(path).unwrap();
    let mut handler = Handler::default();
    parse_utf8(&mut file, &mut handler).expect("Failed to parse file");
    assert_eq!(handler.0, data);

    let mut file = File::open(path).unwrap();
    let mut binary = Vec::new();
    file.read_to_end(&mut binary).expect("Failed to read file");

    let data2 = parse_bytes_utf8_to_map(&binary).expect("Failed to parse file");
    assert_eq!(data2, data);
}

pub fn do_test_iso(path: &str) {
    let mut file = File::open(path).unwrap();
    let java_map = jvm_prop_to_rust_map(path, "ISO-8859-1");
    let data = parse_iso_8859_1_to_map(&mut file).expect("Failed to parse file");

    for (key, val) in data.iter() {
        assert_eq!(java_map.get(key), Some(val), "{}={}", key, val);
    }
    assert_eq!(java_map.len(), data.len());

    let mut file = File::open(path).unwrap();
    let mut handler = Handler::default();
    parse_iso_8859_1(&mut file, &mut handler).expect("Failed to parse file");
    assert_eq!(handler.0, data);

    let mut file = File::open(path).unwrap();
    let mut binary = Vec::new();
    file.read_to_end(&mut binary).expect("Failed to read file");

    let data2 = parse_bytes_iso_8859_1_to_map(&binary).expect("Failed to parse file");
    assert_eq!(data2, data);
}

#[test]
pub fn test() {
    do_test_utf8("./tests/test.properties");
}

#[test]
pub fn t1() {
    do_test_utf8("./tests/t1.properties");
    do_test_iso("./tests/t1.properties");
}

#[test]
pub fn t2() {
    do_test_utf8("./tests/t2.properties");
    do_test_iso("./tests/t2.properties");
}

#[test]
pub fn t3() {
    do_test_utf8("./tests/t3.properties");
    do_test_iso("./tests/t3.properties");
}

#[test]
pub fn t4() {
    do_test_utf8("./tests/t4.properties");
    do_test_iso("./tests/t4.properties");
}

#[test]
pub fn t5() {
    do_test_utf8("./tests/t5.properties");
    do_test_iso("./tests/t5.properties");
}

#[test]
pub fn t6() {
    do_test_utf8("./tests/t6.properties");
    do_test_iso("./tests/t6.properties");
}

#[test]
pub fn t7() {
    do_test_utf8("./tests/t7.properties");
    do_test_iso("./tests/t7.properties");
}
#[test]
pub fn t8() {
    do_test_utf8("./tests/t8.properties");
    do_test_iso("./tests/t8.properties");
}

#[test]
pub fn t9() {
    do_test_utf8("./tests/t9.bin");
}

#[test]
pub fn t10() {
    do_test_utf8("./tests/t10.bin");
}

#[test]
pub fn t11() {
    do_test_utf8("./tests/t11.bin");
}

#[test]
pub fn t12() {
    do_test_utf8("./tests/t12.bin");
}

#[test]
pub fn t13() {
    do_test_utf8("./tests/t13.bin");
}

#[test]
pub fn t14() {
    do_test_utf8("./tests/t14.properties");
    do_test_iso("./tests/t14.properties");
}

#[test]
pub fn t15() {
    do_test_utf8("./tests/t15.properties");
    do_test_iso("./tests/t15.properties");
}