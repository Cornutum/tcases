package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiTest {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, pathResource( "org/cornutum/tcases/openapi/moco/OpenApiTest-Moco.json"));

    @Test
    public void putPost_0() {
        given()
            .queryParam( "postId", "0")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_1() {
        given()
            .queryParam( "postId", "162433803293032905.3")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_2() {
        given()
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_3() {
        given()
            .queryParam( "postId", "")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_4() {
        given()
            .queryParam( "postId", "b;7")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=Not number
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_5() {
        given()
            .queryParam( "postId", "-1")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_0() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_1() {
        given()
            .header( "X-Post-Types", "2345,1001")
            .header( "X-User-Id", "911079684")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_2() {
        given()
            .header( "X-Post-Types", "7700,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_3() {
        given()
            .header( "X-User-Id", "971553612")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_4() {
        given()
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "629242036")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_5() {
        given()
            .header( "X-Post-Types", "-1010.8")
            .header( "X-User-Id", "671196059")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_6() {
        given()
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "162711265")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_7() {
        given()
            .header( "X-Post-Types", "1001,2345,7700")
            .header( "X-User-Id", "605209447")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_8() {
        given()
            .header( "X-Post-Types", ",1001")
            .header( "X-User-Id", "357283292")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_9() {
        given()
            .header( "X-Post-Types", "378.3,rrbrnwbqdufilajc,true,jhqt,-610.5")
            .header( "X-User-Id", "279587789")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_10() {
        given()
            .header( "X-Post-Types", "1066485475,138937070")
            .header( "X-User-Id", "297915772")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_11() {
        given()
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "7116137")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_12() {
        given()
            .header( "X-Post-Types", "1001,7700")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_13() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_14() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", ",@Pxh_%0+")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_15() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

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
            .cookie( "postId", "B|C")
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
            .cookie( "postId", "")
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
            .cookie( "postId", "|C")
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
            .cookie( "postId", "-668.8|402")
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
            .cookie( "postId", "o#4S%*LR[%8dv>d[0uWL@9c%la<^ftwL*Ay50H/5/Te?:IWW>(0&q0F3d-s)B*M.nyRtPrizHbjV/<`k:%bty!RmF/fxM+$Vo8HnAmR(#}-.B{KfmWe[a3lgj=NQ&w+Z&X9ouL2P/AK>.Aknpme%{^8'_ZS>3_]JI^~/56<Y3z0kL]Fu4lx_WI8N_KMyJ7m8:!SVWfszdHa_VX^gq?'KKyWm|u(SRDKpTD.p*jyM['yvZf8~tVJ%CaUh{[*]?tpzgKE.!kpK)&b]06Myj]o[krdG0(^1HxKY+s-Tq.2O#`q3kw%^'`/S&_dd'k>=Q$#z(>r~[[o_+{Ex9M!ciuNBP4PW<fVD1Br7Yy_fBJH>l#Tvir')LlV(/6nojjoye%t$77t8=o*TFF&XEcvt-k%pZB}w'Dy>8B}l#&c#TxF!!EP3m")
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
            .pathParam( "userId", "736602397")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_2() {
        given()
            .pathParam( "[attributes]", "likes=615331116")
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
            .pathParam( "userId", "Z=aRk9 O,X699I,brEZ")
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
            .pathParam( "userId", "526450548")
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
            .pathParam( "userId", "910549170")
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
            .pathParam( "[attributes]", "518.5")
            .pathParam( "userId", "152327052")
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
            .pathParam( "userId", "739021482")
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
            .pathParam( "userId", "281991978")
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
            .pathParam( "[attributes]", "approved=Q[u7+[Y")
            .pathParam( "userId", "771787122")
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
            .pathParam( "userId", "516010045")
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
            .pathParam( "[attributes]", "approved=true,likes=Ml\\")
            .pathParam( "userId", "987701827")
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
            .pathParam( "userId", "359159588")
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
            .pathParam( "[attributes]", "approved=true,bony=ilcplvo,,jvqlvnddxxspydv,VtY<H:$,chmjhsp=true")
            .pathParam( "userId", "1030948493")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
