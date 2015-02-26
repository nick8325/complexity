
class TestJSONSimple {
    public static void main(String[] args) {
        try {
          org.json.simple.parser.JSONParser parser = new org.json.simple.parser.JSONParser();
          Object obj = parser.parse("{\"oyy\":8,\"rvefsjl\":7}");
          System.out.println(obj);
        }
        catch(Exception e) {}
    }
}
