Core philosophy
    KISS
    Complete
    Choose your battle


Top down (UX technical design, integrated)
    - 10 use case to make sure we provide/find sound generic solutions to UI composition
        - Identification of based features (eg. flexbox in 3d)
        - Identification of conceptual fences. (rectangle real estate is mandatory for all cases, 
        real-estate behaviour within is not. eg. Speedometer lives inside a square, has rounded graphics and a radial movement of the needle. Hard to do in html5, actually framework do not extend to such behavior in a streamlined fashion. Can we do better?)
        - Bis, Identification of specialised features (eg. provision for flexible extensibility)
    1. Slack window behaviour (web app language, complex resizing behavior, floating, z behavior (3rd dimension of envisionned flexbox))
    2. Google map on cell phone (continuous updated frame (like 3d rendering inside a game engine editor) + static real estate + dynamic real estate)
    3. Industrial meters etc (spacial/real estate behavior not boxed based)  [should cross findings from 8]
    4. Matplotlib (vertical text, stacking of parameterized graphics)
    5. Gimp (classic desktop UI language and widgets)
    6. VsCode (text editor UI language and widgets ... special needs?)
    7. Website (flowing top down with animations)
    8. Game Hud (really custom graphics, custom behavior eg. star shape expension of widgets with spring like behavior)
    9. Widget Library (Essentials first .... exotic)
    10. ? 

Main structure
    - Stay open to new ideas! Bring them.
    - tree based (Ada std multiway, easily swappable for limited resource boards, eg. fixed size tree) (DOM like)
    - Flexbox 3d ground up. Revisit classic modeling (Already improved in my rudimentary version, IMO)
    - Child subtree event based redraw (performance, not to hog CPU), or else?
    - Keep an eye on tasked process opportunity. (available core based strategies, eg. Desktop vs STM32 LCD display)
    - Bring in full AdaUI idea. (procedural thread of control <==> "invertion")
    - abtract rendering backends. (x11, win32, bare (STM32) etc)
    - Expand on TON idea. (Typed Object Notation)

Akward Needs, to get right(tentative)
    - lerp (Linearly interpolates between two points.) Comes with 
    - timing, game loop like mecanism (fade in, fade out, sliding etc). Register widget in needs of such mecanism or let user implement? (I would love to have basic blocks given to me... at least)
    - Keep an eye on tooling layer posibility, eg. DOM editor, widgets editor etc.
    - Like on Android, stretch whole graphics (fontrs, images etc) globally. reproduce, bring website donw page. try to go further down --> check graphic behavior.

Build-On
    - QOI Ada for images (add other format later). Image format? How does it fit with rendering backends?
    - AdaUI (redo or extract from Gnoga binding) - Investigate (protected objects and tasks viability on barebone)

Priorities
    - Test case, eg. 
        Image filtering 
            DONE: X11 binding, rudimentary flexbox, bitmap font
            WIP : image, resize image
            TODO : button, list selector, CUDA point of sync and external control.
    - Features tests/sandbox cases to harden/control/isolate features development.
    - Available on Alire
    - Support X11, win32 and MacOS