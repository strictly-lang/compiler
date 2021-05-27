describe("match case handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    });
    afterEach(() => {
        container.remove();
    });

    it("with basic case", () => {
        const element = document.createElement("test-components-helper-match-base");

        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].textContent).toBe("first");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");

        element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(new MouseEvent("click"));

        expect(element.shadowRoot.childNodes[0].textContent).toBe("second: 1");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    });

    it("with siblings case", () => {
        const element = document.createElement("test-components-helper-match-siblings");

        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].textContent).toBe("first");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");

        element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(new MouseEvent("click"));

        expect(element.shadowRoot.childNodes[0].textContent).toBe("second: 1");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");

        element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(new MouseEvent("click"));

        expect(element.shadowRoot.childNodes[0].textContent).toBe("third: 3");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    });

    it("with nested case", () => {
        const element = document.createElement("test-components-helper-match-nested");

        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].textContent).toBe("first");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");

        element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(new MouseEvent("click"));

        expect(element.shadowRoot.childNodes[0].textContent).toBe("second nestedValue: 3 siblingValue: 1");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");

        element.shadowRoot.childNodes[0].childNodes[0].dispatchEvent(new MouseEvent("click"));

        expect(element.shadowRoot.childNodes[0].textContent).toBe("third: 6");
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    });
});