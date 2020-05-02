package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiTest {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, pathResource( "org/cornutum/tcases/openapi/testwriter/OpenApiTest-Moco.json"));

    @Test
    public void tracePosts_0() {
        given()
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_1() {
        given()
            .cookie( "postId", "B|A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_2() {
        given()
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_3() {
        given()
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_4() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_5() {
        given()
            .cookie( "postId", "27")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_6() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_7() {
        given()
            .cookie( "postId", "A|C|B")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_8() {
        given()
            .cookie( "postId", "|B")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_9() {
        given()
            .cookie( "postId", "true|ku,7K7B6{2,rnwbqdufilajckkj,~Q.hnf,xjqlolvwyj,70")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_10() {
        given()
            .cookie( "postId", "kj3fr@-+sqM&4-!]o*TX8LZ)o#4S%*LR[%8dv>d[0uWL@9c%la<^ftwL*Ay50H/5/Te?:IWW>(0&q0F3d-s)B*M.nyRtPri|HbjV/<`k:%bty!RmF/fxM+$Vo8HnAmR(#}")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_11() {
        given()
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_0() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=0")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_1() {
        given()
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "1027936426")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_2() {
        given()
            .pathParam( "[attributes]", "likes=599390980")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_3() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_4() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_5() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "qcwmytfrezwpr,:09LC,cpjmzox,134,hkqvrnyertkqjgu,9p=,gtM)")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_6() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "-1")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_7() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "90296549")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_8() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "552115430")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_9() {
        given()
            .pathParam( "[attributes]", "true")
            .pathParam( "userId", "147244857")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_10() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "20336023")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Property-Count=< 1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_11() {
        given()
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "847994705")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_12() {
        given()
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "1049386167")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_13() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=")
            .pathParam( "userId", "421656096")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_14() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=820.9")
            .pathParam( "userId", "58792544")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_15() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=-1")
            .pathParam( "userId", "33450616")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_16() {
        given()
            .pathParam( "[attributes]", "approved=true,zqtjg=,bnwbhlxxtrylgzuj=true")
            .pathParam( "userId", "61915038")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
