package org.cornutum;

import org.cornutum.utils.MyBaseClass;

import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTest extends MyBaseClass {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> {X} #Z")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X} {X} #Z")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "173.0")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> #Z <Y> <Y>")
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
                .baseUri( forTestServer())
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
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "?2Xymv&}H#;=Djkkb=FG+};m0{JacOOE\\zCRKR7+~B|)bY")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "postId", "379165660888365972.1")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "postId", "pgsgeoavqrdzca,NZL1pD5K")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .queryParam( "postId", "366184067174664893.7")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "905393289778670213.3")
                .contentType( "application/json")
                .request().body( "true")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "599224131442586607.5")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "58399049507497127.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "integer", "906")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "143729646859820917.4")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "701889767056057938.2")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "28062116013235394.9")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "g", "fAF27")
                .formParam( "mpplzzsz", "-449")
                .formParam( "hipjpyppo", "true")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "134833453419165181.0")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "874290200367842615.5")
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
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "866978622401110256.1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Y[o()^c].aB:g?$U#;S`<o|wvom%r-(D.4}Xc1G]94bvS?oji}#>7%VabnYfUHN\\\"LrnIUXxmT?7)>)dNIjHS-@:WDdF&]i%qj01)}/WB/:Jw8=,,tDB!l|]S.!=6A\\cplTx5Xa;j*!GmV4l&8J_C:'5s_has%$ASU.QipQ,TsI`zu6;ta,jX[W>IL#[%.n?R_lLTVYL=,0skhoQJrbD.!3-?%@")
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
                .baseUri( forTestServer())
                .queryParam( "postId", "12536855800514899.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
                .formParam( "himevxzt", "a!")
                .formParam( "himevxzt", "n6]E")
                .formParam( "himevxzt", "")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
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
                .baseUri( forTestServer())
                .pathParam( "approved", ".661.2")
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
                .baseUri( forTestServer())
                .pathParam( "approved", ".590664637")
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
    public void putPosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "T")
                .cookie( "region", "a")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"I@g.net\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "|iXKvw{MDZlWT(|.")
                .cookie( "region", "f-V+0l'Eh80Z=$Dp")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"7KVj.uk!bMr$S/-*% jTw/P%.T#~a sHY |UzR3*.>'PQE0^Sjhn!14kQLdhYU;h\",\"email\":\"{h@D.79.cS.0O.a0.ol.iC.C8.5C.net\"}")
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
                .baseUri( forTestServer())
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"J@P.net\"}")
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
                .baseUri( forTestServer())
                .cookie( "postId", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"l@q.com\"}")
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
                .baseUri( forTestServer())
                .cookie( "postId", "999")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"P@x.gov\"}")
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
                .baseUri( forTestServer())
                .cookie( "region", "7")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"C@I.com\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", null)
                .cookie( "region", "|")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"+@H.com\"}")
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
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "f(VJI7K80bO*2?)xT")
                .cookie( "region", "b")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"r@o.org\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "2")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"2@6.com\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "e")
                .cookie( "region", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\";@C.net\"}")
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
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "p")
                .cookie( "region", "%s#U~Ek1*]FO7@*e*")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"I@e.org\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "e")
                .cookie( "region", "{")
                .cookie( "edwogeajtzulqnmw", "")
                .cookie( "cnquwwymqq", "")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"f@n.com\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "T")
                .cookie( "region", "l")
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
                .baseUri( forTestServer())
                .cookie( "country", "j")
                .cookie( "region", "k")
                .contentType( "text/xml")
                .request().body( "[]")
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
                .baseUri( forTestServer())
                .cookie( "country", "C")
                .cookie( "region", "i")
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
                .baseUri( forTestServer())
                .cookie( "country", "~")
                .cookie( "region", "B")
                .contentType( "text/plain")
                .request().body( "")
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
                .baseUri( forTestServer())
                .cookie( "country", "4")
                .cookie( "region", "6")
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
                .baseUri( forTestServer())
                .cookie( "country", "?")
                .cookie( "region", "S")
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
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "U")
                .cookie( "region", "A")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"RY.org\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "e")
                .cookie( "region", "n")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"^A6.Cyq.1/m.XYy.Br/.so8@JG.bo.gov\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "G")
                .cookie( "region", "e")
                .contentType( "text/plain")
                .request().body( "{\"email\":\"O@i.gov\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "2")
                .cookie( "region", "q")
                .contentType( "text/plain")
                .request().body( "{\"text\":null,\"email\":\"Z@X.edu\"}")
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
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "w")
                .cookie( "region", "t")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"=\\\\^:f&k#^>V(jj{2s\\\"JW1M1pv*]h..[m3'1;g`cYe^#tqBJlS7?74*T+5maBymiYW\",\"email\":\"J@w.edu\"}")
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
                .baseUri( forTestServer())
                .cookie( "country", "{")
                .cookie( "region", "[")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"h@A.org\",\"jdyjhbvh\":[\"$s_qZd\",\"|-\"]}")
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

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
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
