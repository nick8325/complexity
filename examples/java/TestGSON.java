
class TestGSON {
    public static void main(String[] args) {
        try {
          com.google.gson.Gson gson = new com.google.gson.Gson();
          String json = "{\"oyy\":8,\"rvefsjl\":{\"ss\" : 123}}";
          Object obj = gson.fromJson(json, Object.class);
          System.out.println(gson.toJson(obj));
        }
        catch(Exception e) {}
    }
}
