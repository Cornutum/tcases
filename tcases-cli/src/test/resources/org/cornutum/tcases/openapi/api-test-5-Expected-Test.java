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
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "884128300094585099.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "%")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "579110988210992054.5")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "100055597218570470.8")
            .contentType( "application/xml")
            .request().body( ">jg-FQI")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "545768800747318227.7")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "787781271506673512.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", "")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "887390451195556957.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "218911377319422868.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "847512139010470218.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "eytx,7ea{")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "711003745143914146.9")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "167771822150204639.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "886308504886987482.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "i&")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "postId", "920816794015899048.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "dxobuuyffc", "true")
            .formParam( "wssfr", "true")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "100|93|41|58")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "27")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "0|85|16|15|7")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "%lB,,iZ7s")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .queryParam( "ids", "0|82|16|16")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "f")
            .cookie( "region", "U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@g.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "3?yf*R>MA(|%pz}@")
            .cookie( "region", "x8U}z3%?N$)|r!FQ")
            .contentType( "text/plain")
            .request().body( "{\"text\":\")@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a_q-M5\",\"email\":\"8rM.?GYI.3Xq$@8g.sOO.YAD.VG3.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"'@2.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@4.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "postId", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"O@T.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "region", ")")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@0.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "")
            .cookie( "region", "F")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@M.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "j'AJKsh61`bvz&f^7")
            .cookie( "region", ".")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"r@F.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "<")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"k@b.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "%")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"r@m.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "m")
            .cookie( "region", "F~v>f|K%35t[U3nn}")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"P@v.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "W")
            .cookie( "region", "0")
            .cookie( "xtrylgzujvaskrsm", "-+%")
            .cookie( "osgrs", "-625.2")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@Q.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "@")
            .cookie( "region", "o")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "q")
            .cookie( "region", "q")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", " (&O;=")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "t")
            .cookie( "region", ".")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", ")")
            .cookie( "region", "H")
            .contentType( "text/plain")
            .request().body( "-463")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "B")
            .cookie( "region", "'")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "U")
            .cookie( "region", "{")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "V")
            .cookie( "region", "q")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"F7.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "Y")
            .cookie( "region", "D")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Ku.#5c._&*.!`~.fPV@j.0c.bm.I4.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "_")
            .cookie( "region", "<")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"F@Y.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "J")
            .cookie( "region", "&")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"+@J.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "#")
            .cookie( "region", "$")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"JPt TLg/#+P9F03USC(F12J)k+BE<&oGTJh<RrmNe6-yTg=G<?qA\\\\MHn+L:c\\\"B7~'\",\"email\":\"q@2.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:9999"))
            .cookie( "country", "O")
            .cookie( "region", "6")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"g@I.org\",\"qlrcnlkwzq\":\"[oIUO\"}")
        .when()
            .request( "PUT", "/posts")
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

    private String forTestServer() {
        return forTestServer( null);
    }

    private String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
