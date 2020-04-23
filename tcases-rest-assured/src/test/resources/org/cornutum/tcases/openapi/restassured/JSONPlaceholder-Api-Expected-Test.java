package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class JSONPlaceholderApiTest {

    @Test
    public void getPosts_0() {
        given()
            .queryParam( "userId", "1")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPosts_1() {
        given()
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPosts_2() {
        given()
            .queryParam( "userId", "10")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPosts_3() {
        given()
            .queryParam( "userId", "")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_4() {
        given()
            .queryParam( "userId", "true")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_5() {
        given()
            .queryParam( "userId", "0")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // userId.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_6() {
        given()
            .queryParam( "userId", "11")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // userId.Value.Is=11
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_0() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_1() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_2() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_3() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_4() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_5() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_6() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_7() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_8() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_9() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_10() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.userId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_11() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_12() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_13() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.userId.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_14() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.userId.Value.Is=11
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_15() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.title.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_16() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.title.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_17() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.title.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_18() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_19() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.body.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_20() {
        given()
        .when()
            .request( "POST", "https://jsonplaceholder.typicode.com/posts")
        .then()
            // Body.application-json.Items.Contains.Value.Properties.body.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsPostId_0() {
        given()
            .pathParam( "post-id", "1")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsPostId_1() {
        given()
            .pathParam( "post-id", "100")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsPostId_2() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsPostId_3() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsPostId_4() {
        given()
            .pathParam( "post-id", "FXu&L%3F!o")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsPostId_5() {
        given()
            .pathParam( "post-id", "0")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsPostId_6() {
        given()
            .pathParam( "post-id", "101")
        .when()
            .request( "DELETE", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=101
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPostsPostId_0() {
        given()
            .pathParam( "post-id", "1")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPostsPostId_1() {
        given()
            .pathParam( "post-id", "100")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPostsPostId_2() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPostsPostId_3() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPostsPostId_4() {
        given()
            .pathParam( "post-id", "true")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPostsPostId_5() {
        given()
            .pathParam( "post-id", "0")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPostsPostId_6() {
        given()
            .pathParam( "post-id", "101")
        .when()
            .request( "GET", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=101
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPostsPostId_0() {
        given()
            .pathParam( "post-id", "1")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPostsPostId_1() {
        given()
            .pathParam( "post-id", "100")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPostsPostId_2() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPostsPostId_3() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPostsPostId_4() {
        given()
            .pathParam( "post-id", "%3EJ%7B_")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPostsPostId_5() {
        given()
            .pathParam( "post-id", "0")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPostsPostId_6() {
        given()
            .pathParam( "post-id", "101")
        .when()
            .request( "PATCH", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=101
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPostsPostId_0() {
        given()
            .pathParam( "post-id", "1")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPostsPostId_1() {
        given()
            .pathParam( "post-id", "100")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPostsPostId_2() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPostsPostId_3() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPostsPostId_4() {
        given()
            .pathParam( "post-id", "")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPostsPostId_5() {
        given()
            .pathParam( "post-id", "0")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPostsPostId_6() {
        given()
            .pathParam( "post-id", "101")
        .when()
            .request( "PUT", "https://jsonplaceholder.typicode.com/posts/{post-id}")
        .then()
            // post-id.Value.Is=101
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
