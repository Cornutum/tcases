package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiTest {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, pathResource( "org/cornutum/tcases/openapi/moco/OpenApiTest-Moco.json"));

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "2,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,2")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "user attributes", (String) null)
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "K")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "user attributes", "")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,true")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,=|e3 ,,NUhF&4hVmkeev1$d?")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,VIP!,o,6v {g\"Y,zaslbheunfwyuvyq,true")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?", (String) null)
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?", "804.4")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", (String) null)
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "true")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", ",2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "cajzqpokvafd,true,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "488236961,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "post?[kh]", "-595")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "{X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "<Y> #Z {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "#Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "{X} {X} {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", (String) null)
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "xmxkgtitztgqum true gloccjduoicfrr Pvt|")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "<Y> <Y> {X} {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "Post Marks", "zk)EW\\\")_rtGQ%<N,1v4CnS:%_;e8ev,!;|9B4~q,;$eHzu$Za-TV1GK:2;k+AX2t-h.Pb;;YB;lr9NKkMx~]gGaITGeP+#iYUNlm:6/A|hN_Hw}yWP\"Ywuk65Npy5U\\5tskpCAWVUS}\\e|")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "672351344895692221.7")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "(?)")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Me+You")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", (String) null)
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "true")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "-1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "558920344373741220.4")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "257909579949629647.3")
                .contentType( "application/xml")
                .request().body( "{\"gvuhccut\":true}")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "418906927138517083.8")
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "155418205231237153.3")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "string", "aJ{jn")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "705561256638661028.4")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "770131052485060506.9")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", (String) null)
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "620250610154060366.0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "579110988210992054.5")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "100055597218570470.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", (String) null)
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "888334915240511264.0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "-FQIS")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "757173325052809070.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "MlyFy,$liP22^%7e(G'7ea{?,nz,z.)[bd|Gb&!4i&(~G$f1)|:\"1pz*4W,M8!_HGME")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "postId", "826597633696147676.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
                .formParam( "jzc", "")
                .formParam( "j", "-492.0")
                .formParam( "cgdnpojyynhy", "ZF:6&H")
                .formParam( "cgdnpojyynhy", "oL")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".1")
                .pathParam( "userId", ".1000")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".pfbn0z")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".-1")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".1001")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=1001
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "approved", ".747903263")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "0")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "100|93|41|58")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", (String) null)
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "27")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "0|85|16|15|7")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Size=5
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "%lB,")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "-1")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "101")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=101
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .queryParam( "ids", "0|0|87|8")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "2345,7700")
                .header( "X-User-Id", "415622582")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "7700,2345")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-User-Id", "93039807")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "")
                .header( "X-User-Id", "738079435")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "")
                .header( "X-User-Id", "627687293")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001")
                .header( "X-User-Id", "273068116")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,2345,7700")
                .header( "X-User-Id", "712375791")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", ",2345")
                .header( "X-User-Id", "9137359")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "ykcjql,qY,eh,0b![,vggtl,@-3mA%,2345")
                .header( "X-User-Id", "339524612")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "-15203243,1001")
                .header( "X-User-Id", "892688793")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,1001")
                .header( "X-User-Id", "980116701")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,7700")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "true")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "-1")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"4@k.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"+[Y{1jvMl\\\\?fP`ZaHE\\\\}tmHpOpb:$|%5.0?^pN_OoNhEW$zT1VtY<H:$J9Ab/K7&\",\"email\":\"z`3Nlq.SfSxwn.~F3a/g@hoZ5MGU.com\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", null)
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "311")
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "text/plain")
                .request().body( "501.3")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "null")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "-227.7")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":{\"fbirwqarld\":\"w uGb\",\"ra\":399,\"lhgjz\":true}}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"vi.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"#n!k'@edaHCij.mxE8ZZA.WzAHShm.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"email\":\";@N.gov\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":null,\"email\":\"*@W.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":[\"t\"],\"email\":\"L@h.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"~!lOsfd%8T4N\\\\W%Sf5Tr|w`=C8;V~x]FkwIVg-us5r0!]cKW/BU<tb$Q2<fbMlh$?\",\"email\":\"=@h.com\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"2@s.net\",\"jbtmqjw\":-857.7,\"ns\":-885}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "%")
                .cookie( "region", "5")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"1@b.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "K-8Vr3?yf*R>MA(|")
                .cookie( "region", "%pz}@x8U}z3%?N$)")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"mwuof)@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a\",\"email\":\"u.S@UvqLEL.2G8ZjGAS.8gsOOYAD.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"x@P.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"f@b.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "434")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"A@I.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "region", "^")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"R@V.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", null)
                .cookie( "region", "L")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"c@Q.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "-525")
                .cookie( "region", "n")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"-@2.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "")
                .cookie( "region", "T")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"?@q.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Value.Length=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "J$7j'AJKsh61`bvz&")
                .cookie( "region", "f")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"{@M.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "i")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"8@T.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "k")
                .cookie( "region", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"Z@m.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "b")
                .cookie( "region", "385")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"6@v.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "=")
                .cookie( "region", "")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"B@s.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Value.Length=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "}")
                .cookie( "region", "tH`mF~v>f|K%35t[U")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"{@C.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "n")
                .cookie( "region", "=")
                .cookie( "bhlxxt", "tilP~bV")
                .cookie( "krsmzryfb", "{1I(pSmy")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"T@D.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "o")
                .cookie( "region", "q")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "q")
                .cookie( "region", "'")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "number", "452.6")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "?")
                .cookie( "region", "y")
                .contentType( "text/plain")
                .request().body( "")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "}")
                .cookie( "region", "x")
                .contentType( "text/plain")
                .request().body( "true")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "X")
                .cookie( "region", "t")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", ".")
                .cookie( "region", ")")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "H")
                .cookie( "region", "N")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":-706.6}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "U")
                .cookie( "region", "{")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"Ik.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "Q")
                .cookie( "region", "9")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"LIP.CS@eutB.C5eEe.YO75D.Dj0cb.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "o")
                .cookie( "region", "(")
                .contentType( "text/plain")
                .request().body( "{\"email\":\"|@G.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "]")
                .cookie( "region", "e")
                .contentType( "text/plain")
                .request().body( "{\"text\":null,\"email\":\"i@z.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "y")
                .cookie( "region", "|")
                .contentType( "text/plain")
                .request().body( "{\"text\":{\"urppkixcjonllycd\":true,\"qxpcqj\":\"SC\",\"gdqx\":\"\"},\"email\":\"o@u.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "P")
                .cookie( "region", "D")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"Jh<RrmNe6-yTg=G<?qA\\\\MHn+L:c\\\"B7~''q!%8\\\\iV!`z?;4Ka\\\"ri&kQGwCp11[oIUO\",\"email\":\"^@z.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "country", "'")
                .cookie( "region", "P")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"J@I.org\",\"m\":true,\"tcfgeleszxltbt\":-623}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "B|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", null)
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "ftyqhpdtdbqf|-509|mwrtsmrqtiqkhvc|433|rizv|80.8")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "A|B|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "wmrnrf,763|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "jrH_o$X`LnQgI'mW?MxXRGcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.5ef?WOZ+>MHIUD-{?yH$5-DM3*T>hR%ct'(7EzCR]8D=vL#m4-h23+#VHsMG[K:u@iQR9D@$=:8GOlaBS5=h=(rixn|B")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .cookie( "postId", "A|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;likes=222686173")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";attributes=true")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";likes=279878075")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved;likes=1006574698")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=121;likes=832858988")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;likes")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;likes=:.-g")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;likes=-1")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;subject;likes=141365157")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;subject=sfcgcqxkuswiqomw,Sm,*Y,ewb,-784.0,nvlzfpbyofmna,-309;likes=405281092")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;subject=;#g33QP&F3Sq{<43[;likes=19340462")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "attributes", ";approved=false;likes=70759329;yzpjpn=-744.5")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true,likes=0")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=false")
                .pathParam( "userId", "679208122")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "likes=990959955")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "-1")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "963498846")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "581525744")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "337.6")
                .pathParam( "userId", "1066915565")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertyCount_Is_Lt_1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "143212479")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Property-Count=< 1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=")
                .pathParam( "userId", "462664869")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=Gmd%s`u")
                .pathParam( "userId", "416406102")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true,likes=")
                .pathParam( "userId", "15743998")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true,likes=,|")
                .pathParam( "userId", "1039847385")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true,likes=-1")
                .pathParam( "userId", "159023870")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
                .pathParam( "[attributes]", "approved=true,whlbni=true,ovghzublzqi=,lqnytsupujyedml=-51.9")
                .pathParam( "userId", "190253101")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getUsers() {
        Response response =
            given()
                .baseUri( forTestServer( "http://localhost:12306"))
            .when()
                .request( "GET", "/users")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/users", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/users", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}
