package org.cornutum.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.Rule;
import com.github.dreamhead.moco.HttpsCertificate;
import static com.github.dreamhead.moco.HttpsCertificate.certificate;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyMocoTest {
    private static final HttpsCertificate myCertificate = certificate( pathResource( "myCert.cks"), "kss!", "css!");

    @Rule
    public MocoJunitRunner runner = MocoJunitRunner.jsonHttpsRunner( 9999, pathResource( "myMocoServerConfig"), myCertificate);

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .queryParam( "postId", "28999950531402111.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .queryParam( "postId", "jg-")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // postId.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .queryParam( "postId", "919575277660755414.0")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .queryParam( "postId", "448997346156574075.8")
            .contentType( "text/plain")
            .request().body( "[\"MlyFy,$\",\"iP22^\"]")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .queryParam( "postId", "571802726739922035.8")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .queryParam( "postId", "180496212748754591.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "682.4")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .queryParam( "postId", "673335475426531261.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .queryParam( "postId", "408953493899394760.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .queryParam( "postId", "704936566657433448.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "[bd|Gb&!")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .queryParam( "postId", "395913240289997151.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .queryParam( "postId", "129624029192585112.5")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .queryParam( "postId", "6066229520155986.0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "~E_{ilb'3*76s/1*rt}aB2-ZF:6&HQoL9#`4:\"7\",Dt6K7BH.fpq-a=|e3 ,,NUhF&4hVmkeev1$d?/ vGu(m6v {g\"YtKRC&!R@O^fH|v&,QP2q>\\EL&:s{t\\'[EZ|t9bR_?<uo(/nD+[y0N#$g*yaTi0q{}X#wb$H9#1=NKr}esL&'I")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "postId", "214226054972517301.1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "i", "occjduoicfrrgd,true")
            .formParam( "ptqdburuoffuq", "fuofhaaejdzc,503.9,u,152.0,ynixryojinfn,i#")
            .formParam( "dxgvbsanhusc", "qy\\` #NX")
        .when()
            .request( "PUT", "http://localhost:9999/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .queryParam( "ids", "100|93|41|58")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .queryParam( "ids", "27")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .queryParam( "ids", "0|85|16|15|7")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .queryParam( "ids", "%lB,,iZ7s")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .queryParam( "ids", "0|0|0|0")
        .when()
            .request( "GET", "http://localhost:9999/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "country", "=")
            .cookie( "region", "=")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\";@p.org\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .cookie( "country", "8Vr3?yf*R>MA(|%p")
            .cookie( "region", "z}@x8U}z3%?N$)|r")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"uof)@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a_q\",\"email\":\"04t.8rM.?GY.I3X.q$m.-c|@O.YA.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"H@F.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"k@f.org\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"A@I.edu\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .cookie( "region", "^")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"R@V.edu\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .cookie( "country", "")
            .cookie( "region", "L")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"c@Q.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .cookie( "country", "J$7j'AJKsh61`bvz&")
            .cookie( "region", "f")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@M.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .cookie( "country", "i")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"8@T.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .cookie( "country", "k")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Z@m.edu\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .cookie( "country", "}")
            .cookie( "region", "tH`mF~v>f|K%35t[U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@C.org\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "n")
            .cookie( "region", "=")
            .cookie( "bhlxx", "true")
            .cookie( "ylgzuj", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@c.gov\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .cookie( "country", "f")
            .cookie( "region", "K")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "country", "-")
            .cookie( "region", "+")
            .contentType( "text/xml")
            .request().body( "-180.9")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .cookie( "country", "I")
            .cookie( "region", "(")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .cookie( "country", "p")
            .cookie( "region", "S")
            .contentType( "text/plain")
            .request().body( "-1012")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "country", "`")
            .cookie( "region", "L")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "country", "G")
            .cookie( "region", "c")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "country", "X")
            .cookie( "region", "t")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"ob.org\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "country", "'")
            .cookie( "region", "U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"I.I._.e.I.L.=.F@7AJ80YeutBC5e.gov\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "country", "}")
            .cookie( "region", "F")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"P@D.net\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "country", "]")
            .cookie( "region", "Z")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"4@2.edu\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "country", "@")
            .cookie( "region", "G")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"H`|VKsrWoge0ICD|JPt TLg/#+P9F03USC(F12J)k+BE<&oGTJh<RrmNe6-yTg=G<\",\"email\":\"Q@y.org\"}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "i")
            .cookie( "region", "3")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"F@5.edu\",\"clgvcbvnaj\":{\"aq\":[]},\"rcnlkwzqpejqpnfd\":{\"jivkhk\":[\"9 O X\",\"\"],\"c\":true},\"eleszxltbtohx\":true}")
        .when()
            .request( "PUT", "http://localhost:9999/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
